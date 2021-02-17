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

let config ~root = Irmin_pack.config root

type 'a client = { ipfs : Ipfs.t }

module Conn = struct
  module type S = sig
    val ipfs : Ipfs.t
  end

  module Default = struct
    let ipfs = !Ipfs.default
  end
end

module type S = sig
  include Irmin.S

  val reconstruct_index : ?output:string -> Irmin.config -> unit

  val flush : repo -> unit
  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result
end

module Make_ext
    (Conn : Conn.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
struct
  module X = struct
    module Hash : Irmin.Hash.S with type t = Ipfs.Cid.t = struct
      type t = [ `Cid of string ]

      let t = Irmin.Type.(map string (fun x -> `Cid x) (fun (`Cid x) -> x))

      let short_hash x = Hashtbl.hash x

      let hash_size = 46

      module Cache = Irmin.Private.Lru.Make (struct
        type t = string

        let hash = Hashtbl.hash

        let equal = String.equal
      end)

      let cache = Cache.create 64

      let hash' ipfs f =
        (* TODO: make this work without making HTTP request *)
        let buf = Buffer.create 64 in
        let () = f (Buffer.add_string buf) in
        let s = Buffer.contents buf in
        if Cache.mem cache s then Cache.find cache s
        else
          let hash = Ipfs.hash' ipfs (Buffer.contents buf) in
          let () = Cache.add cache s hash in
          hash

      let hash x = hash' Conn.ipfs x
    end

    module Node' = Irmin.Private.Node.Make (Hash) (P) (M)
    module Commit' = Irmin.Private.Commit.Make (Hash)

    type 'a value = { hash : Hash.t; magic : char; v : 'a } [@@deriving irmin]

    module Version = struct
      let io_version = `V2
    end

    module Index = Irmin_pack.Index.Make (Hash)
    module Pack = Irmin_pack.Pack.File (Index) (Hash) (Version)

    module Contents = struct
      module CA = struct
        module Key = Hash
        module Val = C

        type 'a t = 'a client

        type key = Hash.t

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
      module CA = Irmin_pack.Inode.Make (Config) (Hash) (Pack) (Node')
      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = Hash
        module Val = Commit'

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (Hash) (Val)

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
      module Val = Hash
      include Irmin_pack.Atomic_write (Key) (Val) (Version)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (Hash) (B)

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

      let close t =
        Index.close t.index;
        let* () = Node.CA.close (snd (node_t t)) in
        let* () = Branch.close t.branch in
        Commit.CA.close (snd (commit_t t))

      let flush t = Commit.CA.flush ~index:true (snd (commit_t t))

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
           called to repair the store. *)
        (f := fun () -> Commit.CA.flush ~index:true commit);
        let x = { contents; node; commit; branch; config; index } in
        Gc.finalise (fun x -> flush x) x;
        x

      (*Commit.CA.close (snd (commit_t t)) >>= fun () -> Branch.close t.branch*)

      (** Stores share instances so one clear is enough. *)

      (*let clear t = Contents.CA.clear (contents_t t)*)

      module Dict = Irmin_pack.Dict.Make (Version)

      module Reconstruct_index = struct
        let pp_hash = Irmin.Type.pp Hash.t

        let decode_contents =
          Irmin.Type.(unstage (decode_bin (value_t Contents.Val.t)))

        let decode_commit =
          Irmin.Type.(unstage (decode_bin (value_t Commit.Val.t)))

        let decode_key = Irmin.Type.(unstage (decode_bin Hash.t))

        let decode_magic = Irmin.Type.(unstage (decode_bin char))

        let decode_buffer ~progress ~total pack dict index =
          let decode_len buf magic =
            try
              let len =
                match magic with
                | 'B' -> decode_contents buf 0 |> fst
                | 'C' -> decode_commit buf 0 |> fst
                | 'N' | 'I' ->
                    let hash off =
                      let buf =
                        Irmin_pack.Private.IO.Unix.read_buffer
                          ~chunk:Hash.hash_size ~off pack
                      in
                      decode_key buf 0 |> snd
                    in
                    let dict = Dict.find dict in
                    Node.CA.decode_bin ~hash ~dict buf 0
                | _ -> failwith "unexpected magic char"
              in
              Some len
            with
            | Invalid_argument msg when msg = "index out of bounds" -> None
            | Invalid_argument msg when msg = "String.blit / Bytes.blit_string"
              ->
                None
          in
          let decode_entry buf off =
            let off_k, k = decode_key buf 0 in
            assert (off_k = Hash.hash_size);
            let off_m, magic = decode_magic buf off_k in
            assert (off_m = Hash.hash_size + 1);
            match decode_len buf magic with
            | Some len ->
                let new_off = Int64.add off @@ Int64.of_int len in
                Log.debug (fun l ->
                    l "k = %a (off, len, magic) = (%Ld, %d, %c)" pp_hash k off
                      len magic);
                Index.add index k (off, len, magic);
                progress (Int64.of_int len);
                Some new_off
            | None -> None
          in
          let rec read_and_decode ?(retries = 1) off =
            Log.debug (fun l ->
                l "read_and_decode retries %d off %Ld" retries off);
            let chunk = 64 * 10 * retries in
            let buf = Irmin_pack.Private.IO.Unix.read_buffer ~chunk ~off pack in
            match decode_entry buf off with
            | Some new_off -> new_off
            | None ->
                (* the biggest entry in a tezos store is a blob of 54801B *)
                if retries > 90 then
                  failwith "too many retries to read data, buffer size = 57600B"
                else (read_and_decode [@tailcall]) ~retries:(retries + 1) off
          in
          let rec read_buffer off =
            if off >= total then ()
            else
              let new_off = read_and_decode off in
              (read_buffer [@tailcall]) new_off
          in
          read_buffer 0L

        let reconstruct ?output config =
          if Pack_config.readonly config then
            raise Irmin_pack.Private.IO.Unix.RO_Not_Allowed;
          Log.info (fun l ->
              l "[%s] reconstructing index" (Pack_config.root config));
          let root = Pack_config.root config in
          let dest = match output with Some path -> path | None -> root in
          let log_size = Pack_config.index_log_size config in
          let index = Index.v ~fresh:true ~readonly:false ~log_size dest in
          let pack_file = Filename.concat root "store.pack" in
          let pack =
            Irmin_pack.Private.IO.Unix.v ~fresh:false ~readonly:true
              ~version:(Some Version.io_version) pack_file
          in
          let dict = Dict.v ~fresh:false ~readonly:true root in
          let total = Irmin_pack.Private.IO.Unix.offset pack in
          let bar, progress =
            Irmin_pack.Private.Utils.Progress.counter ~total
              ~sampling_interval:100 ~message:"Reconstructing index"
              ~pp_count:Irmin_pack.Private.Utils.pp_bytes ()
          in
          decode_buffer ~progress ~total pack dict index;
          Index.close index;
          Irmin_pack.Private.IO.Unix.close pack;
          Dict.close dict;
          Irmin_pack.Private.Utils.Progress.finalise bar
      end
    end
  end

  let integrity_check ?ppf ~auto_repair t =
    let module Checks = Irmin_pack.Store.Checks (X.Index) in
    let nodes = X.Repo.node_t t |> snd in
    let commits = X.Repo.commit_t t |> snd in
    let check ~kind ~offset ~length k =
      match kind with
      | `Contents -> Ok ()
      | `Node -> X.Node.CA.integrity_check ~offset ~length k nodes
      | `Commit -> X.Commit.CA.integrity_check ~offset ~length k commits
    in
    Checks.integrity_check ?ppf ~auto_repair ~check t.index

  let reconstruct_index = X.Repo.Reconstruct_index.reconstruct

  let flush = X.Repo.flush

  include Irmin.Of_private (X)
end

module Make
    (Conn : Conn.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
  Make_ext (Conn) (M) (C) (P) (B)
module KV (Conn : Conn.S) (C : Irmin.Contents.S) =
  Make (Conn) (Irmin.Metadata.None) (C) (Irmin.Path.String_list)
    (Irmin.Branch.String)
module Default = KV (Conn.Default) (Irmin.Contents.String)
