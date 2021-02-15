open Lwt.Syntax
module Ipfs = Ipfs
module Crypto = Crypto
module Key = Crypto.Key

module Config = struct
  let stable_hash = 32

  let entries = 128
end

module Value = struct
  type t = Encrypted of string | Plaintext of string [@@deriving irmin]

  let merge = Irmin.Merge.(option (idempotent t))
end

module Store = Irmin_pack.KV (Config) (Value)

type t = { ipfs : Ipfs.t; repo : Store.Repo.t; key : Key.t }

let v ?(ipfs = Ipfs.default) ~key ~root =
  let config = Irmin_pack.config root in
  let+ repo = Store.Repo.v config in
  { key; repo; ipfs }

let init ?ipfs ~root = v ?ipfs ~key:(Key.gen ()) ~root

module Encrypted = struct
  let create t filename =
    Crypto.with_encrypted_file ~key:t.key filename (fun tmp ->
        Ipfs.add t.ipfs ~filename:tmp)

  let fetch t hash =
    let+ data = Ipfs.cat t.ipfs hash in
    match data with
    | Ok data ->
        let s = Crypto.decrypt ~key:t.key data in
        Ok s
    | Error e -> Error e

  let download t ~output hash =
    let* x = fetch t hash in
    match x with
    | Ok (Some s) -> (
        match Crypto.decrypt ~key:t.key s with
        | Some x ->
            let* () = Lwt_io.chars_to_file output (Lwt_stream.of_string x) in
            Lwt.return_ok ()
        | None -> Lwt.return_error (`Not_found (Ipfs.Cid.to_string hash)))
    | Ok None -> Lwt.return_error `Invalid_key
    | Error e -> Lwt.return_error (e :> Error.t)
end

module Plaintext = struct
  let create t filename = Ipfs.add t.ipfs ~filename

  let fetch t hash = Ipfs.cat t.ipfs hash

  let download t ~output hash =
    let* x = fetch t hash in
    match x with
    | Ok s ->
        let* () = Lwt_io.chars_to_file output (Lwt_stream.of_string s) in
        Lwt.return_ok ()
    | Error e -> Lwt.return_error e
end
