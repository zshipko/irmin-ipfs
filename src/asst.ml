open Lwt.Syntax
module Ipfs = Ipfs
module Crypto = Crypto
module Key = Crypto.Key

type t = { key : Key.t }

let v key = { key }

let init () = v (Key.gen ())

module Object = struct
  let create t filename =
    Crypto.with_encrypted_file ~key:t.key filename (fun tmp -> Ipfs.add tmp)

  let get t hash =
    let+ data = Ipfs.get hash in
    match data with
    | Ok data ->
        let s = Crypto.decrypt ~key:t.key data in
        Ok s
    | Error e -> Error e
end
