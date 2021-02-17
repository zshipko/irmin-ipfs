open Lwt.Syntax
open Cryptokit.Cipher

module Key = struct
  type t = Key : string -> t

  let gen ?(len = 32) () = Key (Cryptokit.Random.(string secure_rng) len)

  let of_file ~filename =
    let+ x = Lwt_io.chars_of_file filename |> Lwt_stream.to_string in
    Key x

  let to_file (Key t) ~filename =
    Lwt_io.chars_to_file filename (Lwt_stream.of_string t)
end

let pad = Cryptokit.Padding._8000

let decrypt ~key:(Key.Key key) data : string option =
  try Some (Cryptokit.transform_string (aes ~pad key Decrypt) data)
  with Cryptokit.Error _ -> None

let encrypt ~key:(Key.Key key) data : string =
  Cryptokit.transform_string (aes ~pad key Encrypt) data

let with_encrypted_file ~key original f =
  let name, file = Filename.open_temp_file (Filename.basename original) "enc" in
  let* s = Lwt_io.chars_of_file original |> Lwt_stream.to_string in
  let out = encrypt ~key s in
  output_string file out;
  close_out file;
  let* res = f name in
  let* () = Lwt_unix.unlink name in
  Lwt.return res
