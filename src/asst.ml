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

type t = { repo : Store.Repo.t; key : Key.t }

let v ~key ~root =
  let config = Irmin_pack.config root in
  let+ repo = Store.Repo.v config in
  { key; repo }

let init ~root = v ~key:(Key.gen ()) ~root

module Encrypted = struct
  let create t filename =
    Crypto.with_encrypted_file ~key:t.key filename (fun tmp -> Ipfs.add tmp)

  let fetch t hash =
    let+ data = Ipfs.fetch hash in
    match data with
    | Ok data ->
        let s = Crypto.decrypt ~key:t.key data in
        Ok s
    | Error e -> Error e

  let get t ~output hash =
    let tmp = Filename.temp_file (Filename.basename output) "enc" in
    let* x = Ipfs.get ~output:tmp hash in
    match x with
    | Ok () -> (
        let* s = Lwt_io.chars_of_file tmp |> Lwt_stream.to_string in
        match Crypto.decrypt ~key:t.key s with
        | Some x ->
            let* () = Lwt_io.chars_to_file output (Lwt_stream.of_string x) in
            Lwt.return_ok ()
        | None -> Lwt.return_error (`Not_found (Ipfs.Hash.to_string hash)))
    | Error e -> Lwt.return_error e
end

module Plaintext = struct
  let create _t filename = Ipfs.add filename

  let fetch _t hash = Ipfs.fetch hash

  let get _t hash = Ipfs.get hash
end
