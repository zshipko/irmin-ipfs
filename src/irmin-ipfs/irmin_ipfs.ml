open Lwt.Syntax
open Lwt.Infix
module Ipfs = Ipfs

let src = Logs.Src.create "irmin-ipfs" ~doc:"Irmin IPFS"

let ( // ) = Filename.concat

(* TODO: locking *)

module Log = (val Logs.src_log src : Logs.LOG)

let rec mkdir_all ?(mode = 0o755) path =
  let parent = Filename.dirname path in
  let* parent_exists = Lwt_unix.file_exists parent in
  let* () =
    if not parent_exists then mkdir_all ~mode parent else Lwt.return_unit
  in
  Lwt.catch
    (fun () -> Lwt_unix.mkdir path mode)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
      | exn -> raise exn)

let spec = Irmin.Backend.Conf.Spec.v "ipfs"

let root = Irmin.Backend.Conf.root spec

let config ~root:r =
  let cfg = Irmin.Backend.Conf.empty spec in
  let cfg = Irmin.Backend.Conf.add cfg root r in
  cfg

type 'a client = { ipfs : Ipfs.t }

module Conn = struct
  module type S = sig
    val ipfs : Ipfs.t
  end

  module Default = struct
    let ipfs = !Ipfs.default
  end
end

module type S = Irmin.S

module Hash (Conn : Conn.S) : Irmin.Hash.S with type t = Ipfs.Cid.t = struct
  type t = [ `Cid of string ]

  let t = Irmin.Type.(map string (fun x -> `Cid x) (fun (`Cid x) -> x))

  let short_hash x = Hashtbl.hash x

  let hash_size = 46

  module Cache = Irmin.Backend.Lru.Make (struct
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

module Make_ext
    (Conn : Conn.S)
    (Schema : Irmin.Schema.S with type Hash.t = Ipfs.Cid.t) =
struct
  module X = struct
    module Hash = Hash (Conn)
    module Schema = Schema
    module Node' = Irmin.Node.Make (Hash) (Schema.Path) (Schema.Metadata)
    module Commit_maker = Irmin.Commit.Maker (Schema.Info)
    module Commit' = Commit_maker.Make (Hash)

    type 'a value = { hash : Hash.t; magic : char; v : 'a } [@@deriving irmin]

    module Contents = struct
      module CA = struct
        module Key = Hash
        module Val = Schema.Contents

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

        let close _t = Lwt.return_unit
      end

      let v () = { ipfs = Conn.ipfs }

      include Irmin.Contents.Store (CA) (Hash) (Schema.Contents)
    end

    type 'a store = { root : string; ipfs : Ipfs.t }

    module CA_inner
        (Key : Irmin.Type.S with type t = Ipfs.Cid.t)
        (Val : Irmin.Type.S) (Name : sig
          val name : string
        end) =
    struct
      let unsafe_add_s ~path value =
        Lwt_io.chars_to_file path (Lwt_stream.of_string value)

      let to_raw = Irmin.Type.(unstage @@ to_bin_string Val.t)

      let of_raw = Irmin.Type.(unstage @@ of_bin_string Val.t)

      let unsafe_add t key value =
        let key = Ipfs.Cid.to_string key in
        let value = to_raw value in
        let path = t.root // Name.name // key in
        unsafe_add_s ~path value

      let add t value =
        let value = to_raw value in
        let hash = Ipfs.hash' t.ipfs value in
        let key = Ipfs.Cid.to_string hash in
        let path = t.root // Name.name // key in
        let+ () = unsafe_add_s ~path value in
        hash

      let find t key =
        let key = Ipfs.Cid.to_string key in
        let path = t.root // Name.name // key in
        let* exists = Lwt_unix.file_exists path in
        if exists then
          let+ s = Lwt_io.chars_of_file path |> Lwt_stream.to_string in
          match of_raw s with Ok x -> Some x | _ -> None
        else Lwt.return_none

      let mem t key =
        let key = Ipfs.Cid.to_string key in
        let path = t.root // Name.name // key in
        Lwt_unix.file_exists path

      let batch x f = f (Obj.magic x)

      let clear _ = Lwt.return_unit

      let close _ = Lwt.return_unit
    end

    module Node = struct
      module CA = struct
        module Key = Hash
        module Val = Node'

        type 'a t = 'a store

        type key = Key.t

        type value = Val.t

        include
          CA_inner (Key) (Val)
            (struct
              let name = "node"
            end)
      end

      include
        Irmin.Node.Store (Contents) (CA) (Hash) (Node') (Schema.Metadata)
          (Schema.Path)
    end

    module Commit = struct
      module CA = struct
        module Key = Hash
        module Val = Commit'

        type 'a t = 'a store

        type key = Key.t

        type value = Val.t

        include
          CA_inner (Key) (Val)
            (struct
              let name = "commit"
            end)
      end

      include Irmin.Commit.Store (Schema.Info) (Node) (CA) (Hash) (Commit')
    end

    module Branch = struct
      module Key = Schema.Branch

      module Val = struct
        include Hash

        type hash = t

        let to_hash x = x
      end

      module Name = struct
        let name = "branch"
      end

      module W = Irmin.Backend.Watch.Make (Key) (Val)

      type watch = W.watch

      type t = { w : W.t; store : unit store }

      let v store =
        let w = W.v () in
        { w; store }

      type key = Key.t

      type value = Val.t

      let find t key =
        let key = Irmin.Type.to_string Key.t key in
        let path = t.store.root // Name.name // key in
        let* exists = Lwt_unix.file_exists path in
        if exists then
          let+ s = Lwt_io.chars_of_file path |> Lwt_stream.to_string in
          Some (Ipfs.Cid.of_string s)
        else Lwt.return_none

      let mem t key =
        let key = Irmin.Type.to_string Key.t key in
        let path = t.store.root // Name.name // key in
        Lwt_unix.file_exists path

      let clear _ = Lwt.return_unit

      let close _ = Lwt.return_unit

      let watch_key t key ?init f = W.watch_key t.w key ?init f

      let watch t ?init f = W.watch t.w ?init f

      let unwatch t id = W.unwatch t.w id

      let list t =
        let path = t.store.root // Name.name in
        Lwt_unix.files_of_directory path
        |> Lwt_stream.filter_map (fun x ->
               if String.length x < 1 || x.[0] = '.' then None
               else
                 match Irmin.Type.of_string Schema.Branch.t x with
                 | Ok x -> Some x
                 | _ -> None)
        |> Lwt_stream.to_list

      let remove t key =
        let key = Irmin.Type.to_string Key.t key in
        let path = t.store.root // Name.name // key in
        Lwt_unix.unlink path

      let set t key value =
        let key = Irmin.Type.to_string Key.t key in
        let path = t.store.root // Name.name // key in
        let value = Ipfs.Cid.to_string value in
        Lwt_io.chars_to_file path (Lwt_stream.of_string value)

      let eq = Irmin.Type.(unstage @@ equal (option Val.t))

      let test_and_set t key ~test ~set:s =
        let* a = find t key in
        if eq a test then
          match s with
          | Some s ->
              let* () = set t key s in
              Lwt.return_true
          | None ->
              let* () = remove t key in
              Lwt.return_true
        else Lwt.return_false
    end

    module Slice = Irmin.Backend.Slice.Make (Contents) (Node) (Commit)
    module Remote = Irmin.Backend.Remote.None (Hash) (Schema.Branch)

    module Repo = struct
      type t = {
        config : Irmin.Backend.Conf.t;
        contents : Irmin.Perms.read Contents.CA.t;
        node : Irmin.Perms.read Node.CA.t;
        commit : Irmin.Perms.read Commit.CA.t;
        branch : Branch.t;
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
        let root = Irmin.Backend.Conf.get config root in
        let contents = Contents.v () in
        let store = { ipfs = Conn.ipfs; root } in
        let node = store in
        let commit = store in
        let branch = Branch.v store in
        let* () = mkdir_all (root // "node") in
        let* () = mkdir_all (root // "commit") in
        let+ () = mkdir_all (root // "branch") in
        { contents; node; commit; branch; config }

      let close t = Branch.clear t.branch
    end

    module Node_portable = Irmin.Node.Portable.Of_node (Node')
  end

  include Irmin.Of_backend (X)
end

module Make
    (Conn : Conn.S)
    (Schema : Irmin.Schema.S with type Hash.t = Ipfs.Cid.t) =
  Make_ext (Conn) (Schema)

module KV (Conn : Conn.S) (C : Irmin.Contents.S) =
  Make
    (Conn)
    (struct
      module Info = Irmin.Info.Default
      module Metadata = Irmin.Metadata.None
      module Branch = Irmin.Branch.String
      module Path = Irmin.Path.String_list
      module Contents = C
      module Hash = Hash (Conn)
    end)

module Default = KV (Conn.Default) (Irmin.Contents.String)
