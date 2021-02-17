open Lwt.Syntax
open Lwt.Infix
module Ipfs = Ipfs

module Config = struct
  let stable_hash = 32

  let entries = 256
end

let src = Logs.Src.create "irmin-ipfs" ~doc:"Irmin IPFS"

module Log = (val Logs.src_log src : Logs.LOG)

module Pack_config = Irmin_pack.Config
module Index = Irmin_pack.Index

module Cid = struct
  type t = [ `Cid of string ]

  let t = Irmin.Type.(map string (fun x -> `Cid x) (fun (`Cid x) -> x))

  let short_hash x = Hashtbl.hash x

  let hash_size = 46

  let hash' ipfs f =
    let buf = Buffer.create 64 in
    f (Buffer.add_string buf);
    (* TODO: hash this without making HTTP request *)
    Ipfs.hash' ipfs (Buffer.contents buf)

  let hash = hash' Ipfs.default
end

let config ~root = Irmin_pack.config root

type 'a client = { ipfs : Ipfs.t }

module Conn = struct
  module type S = sig
    val ipfs : Ipfs.t
  end

  module Default = struct
    let ipfs = Ipfs.default
  end
end

module Make_ext
    (Conn : Conn.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S with type t = Cid.t)
    (Node : Irmin.Private.Node.S
              with type metadata = M.t
               and type hash = H.t
               and type step = P.step)
    (Commit : Irmin.Private.Commit.S with type hash = H.t) =
struct
  module X = struct
    module Hash = struct
      include Cid

      let hash = hash' Conn.ipfs
    end

    type 'a value = { hash : H.t; magic : char; v : 'a } [@@deriving irmin]

    module Version = struct
      let io_version = `V2
    end

    module Index = Irmin_pack.Index.Make (H)
    module Pack = Irmin_pack.Pack.File (Index) (H) (Version)

    module Contents = struct
      module CA = struct
        module Key = Cid
        module Val = C

        type 'a t = 'a client

        type key = H.t

        type value = Val.t

        let clear _ = Lwt.return_unit

        let add t value =
          let s = Irmin.Type.to_string Val.t value in
          Ipfs.add ~name:"" t.ipfs s >|= Result.get_ok

        let unsafe_add t _k v = add t v >|= fun _ -> ()

        let find t key =
          Ipfs.cat t.ipfs key >|= function
          | Ok x -> (
              match Irmin.Type.of_string Val.t x with
              | Ok x -> Some x
              | Error _ -> None)
          | Error _ -> None

        let mem t key = find t key >|= Option.is_some

        let batch t f = f (Obj.magic t)
      end

      let v () = { ipfs = Conn.ipfs }

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module CA = Irmin_pack.Inode.Make (Config) (H) (Pack) (Node)
      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = H
        module Val = Commit

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let value = value_t Val.t

          let magic = 'C'

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, v = decode_value s off in
            v.v

          let magic _ = magic
        end)

        include Irmin_pack.Private.Closeable.Content_addressable (CA_Pack)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      include Irmin_pack.Atomic_write (Key) (Val) (Version)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Irmin.Private.Conf.t;
        contents : Irmin.Perms.read Contents.CA.t;
        node : Irmin.Perms.read Node.CA.t;
        commit : Irmin.Perms.read Commit.CA.t;
        branch : Branch.t;
        index : Index.t;
      }

      let contents_t t : 'a Contents.t = t.contents

      let node_t t : 'a Node.t = (contents_t t, t.node)

      let commit_t t : 'a Commit.t = (node_t t, t.commit)

      let branch_t t = t.branch

      let batch t f =
        Commit.CA.batch t.commit (fun commit ->
            Node.CA.batch t.node (fun node ->
                Contents.CA.batch t.contents (fun contents ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit)))

      let v config =
        let root = Pack_config.root config in
        let fresh = Pack_config.fresh config in
        let lru_size = Pack_config.lru_size config in
        let readonly = Pack_config.readonly config in
        let log_size = Pack_config.index_log_size config in
        let throttle = Pack_config.merge_throttle config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~flush_callback:(fun () -> !f ())
              (* backpatching to add pack flush before an index flush *)
            ~fresh ~readonly ~throttle ~log_size root
        in
        let contents = Contents.v () in
        let* node = Node.CA.v ~fresh ~readonly ~lru_size ~index root in
        let* commit = Commit.CA.v ~fresh ~readonly ~lru_size ~index root in
        let+ branch = Branch.v ~fresh ~readonly root in
        (* Stores share instances in memory, one flush is enough. In case of a
           system crash, the flush_callback might not make with the disk. In
           this case, when the store is reopened, [integrity_check] needs to be
           called to repair the store.
           (f := fun () -> (*Contents.CA.flush ~index:false contents*) ()); *)
        { contents; node; commit; branch; config; index }

      let close t =
        Index.close t.index;
        (*Contents.CA.close (contents_t t) >>= fun () ->*)
        Node.CA.close (snd (node_t t))

      (*Commit.CA.close (snd (commit_t t)) >>= fun () -> Branch.close t.branch*)

      (** Stores share instances so one clear is enough. *)

      (*let clear t = Contents.CA.clear (contents_t t)*)
    end
  end

  include Irmin.Of_private (X)
end

module Make
    (Conn : Conn.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
  Make_ext (Conn) (M) (C) (P) (B) (Cid)
    (Irmin.Private.Node.Make (Cid) (P) (M))
    (Irmin.Private.Commit.Make (Cid))
module KV (Conn : Conn.S) (C : Irmin.Contents.S) =
  Make (Conn) (Irmin.Metadata.None) (C) (Irmin.Path.String_list)
    (Irmin.Branch.String)
module Default = KV (Conn.Default) (Irmin.Contents.String)
