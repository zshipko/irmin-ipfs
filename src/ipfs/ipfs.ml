open Lwt.Syntax

type error = [ `Msg of string | `Not_found of string | `Invalid_key ]

module Cid = struct
  type t = [ `Cid of string ]

  let to_string (`Cid h) = h

  let of_string s = `Cid s
end

type t = { uri : Uri.t }

let v ~url = { uri = Uri.of_string url }

let default = ref (v ~url:"http://127.0.0.1:5001")

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let () = at_exit Curl.global_cleanup

let url ?(query = []) t p =
  let p = if String.length p > 0 && p.[0] <> '/' then "/" ^ p else p in
  let p = Uri.with_path t.uri ("/api/v0" ^ p) in
  Uri.with_query' p query

(*let reader file maxBytes =
  let buffer = Bytes.create maxBytes in
  let readBytes = input file buffer 0 maxBytes in
  if readBytes = 0 then "" else Bytes.sub_string buffer 0 readBytes*)

let writer accum data =
  Buffer.add_string accum data;
  String.length data

let handle = function
  | Ok Curl.CURLE_OK -> Ok ()
  | Ok e | Error e -> Error (`Msg (Curl.strerror e))

let build_request ?files ?file_data ?(mode = `POST) ?output url =
  let req = Curl.init () in
  let () = Curl.set_url req (Uri.to_string url) in
  let () =
    match mode with
    | `POST -> Curl.set_post req true
    | `PUT -> Curl.set_put req true
    | _ -> ()
  in
  let () =
    match files with
    | Some files ->
        let files =
          List.map
            (fun filename ->
              Curl.CURLFORM_FILECONTENT (filename, filename, Curl.DEFAULT))
            files
        in
        Curl.set_httppost req files
    | None -> ()
  in
  let () =
    match file_data with
    | Some files ->
        let files =
          List.map
            (fun (name, data) ->
              Curl.CURLFORM_CONTENT (name, data, Curl.DEFAULT))
            files
        in
        Curl.set_httppost req files
    | None -> ()
  in
  let () =
    match output with
    | Some output -> Curl.set_writefunction req (writer output)
    | None -> ()
  in
  req

let request ?files ?file_data ?(mode = `POST) ?output url =
  let req = build_request ?files ?file_data ~mode ?output url in
  let+ code =
    Lwt.catch
      (fun () ->
        let* x = Curl_lwt.perform req in
        Lwt.return_ok x)
      (function
        | Curl.CurlException (code, _, _) -> Lwt.return_error code
        | exn -> raise exn)
  in
  Curl.cleanup req;
  handle code

let hash t data =
  let output = Buffer.create 256 in
  let url = url t ~query:[ ("only-hash", "true") ] "/add" in
  let+ res = request ~output ~file_data:[ ("hash", data) ] url in
  Result.map
    (fun _ ->
      let json = Ezjsonm.from_string (Buffer.contents output) in
      Ezjsonm.find json [ "Hash" ] |> Ezjsonm.get_string |> Cid.of_string)
    res

let hash' t data =
  let output = Buffer.create 256 in
  let url = url t ~query:[ ("only-hash", "true") ] "/add" in
  let req = build_request ~output ~file_data:[ ("hash", data) ] url in
  Curl.perform req;
  Curl.cleanup req;
  let json = Ezjsonm.from_string (Buffer.contents output) in
  Ezjsonm.find json [ "Hash" ] |> Ezjsonm.get_string |> Cid.of_string

let add_file t ~filename =
  let output = Buffer.create 256 in
  let url = url t "/add" in
  let+ res = request ~output ~files:[ filename ] url in
  Result.map
    (fun _ ->
      let json = Ezjsonm.from_string (Buffer.contents output) in
      Ezjsonm.find json [ "Hash" ] |> Ezjsonm.get_string |> Cid.of_string)
    res

let add t ?(name = "") s =
  let output = Buffer.create 256 in
  let url = url t "/add" in
  let+ res = request ~output ~file_data:[ (name, s) ] url in
  Result.map
    (fun _ ->
      let json = Ezjsonm.from_string (Buffer.contents output) in
      Ezjsonm.find json [ "Hash" ] |> Ezjsonm.get_string |> Cid.of_string)
    res

let cat t hash =
  let output = Buffer.create 256 in
  let url = url ~query:[ ("arg", Cid.to_string hash) ] t "/cat" in
  let+ res = request ~output url in
  match res with
  | Ok () -> Ok (Buffer.contents output)
  | Error _ -> Error (`Not_found (Cid.to_string hash))

let get t hash =
  let url = url ~query:[ ("arg", Cid.to_string hash) ] t "/get" in
  request url

let download t ~output hash : (unit, error) result Lwt.t =
  let* x = cat t hash in
  match x with
  | Ok s ->
      let* () = Lwt_io.chars_to_file output (Lwt_stream.of_string s) in
      Lwt.return_ok ()
  | Error e -> Lwt.return_error e

module Encrypted = struct
  module Secret = Crypto.Key

  let add t ~secret ?name data =
    let s = Crypto.encrypt ~key:secret data in
    add t ?name s

  let add_file t ~secret ~filename =
    Crypto.with_encrypted_file ~key:secret filename (fun tmp ->
        add_file t ~filename:tmp)

  let cat t ~secret hash =
    let+ data = cat t hash in
    match data with
    | Ok data ->
        let s = Crypto.decrypt ~key:secret data in
        Ok s
    | Error e -> Error e

  let download t ~secret ~output hash =
    let* x = cat t ~secret hash in
    match x with
    | Ok (Some s) -> (
        match Crypto.decrypt ~key:secret s with
        | Some x ->
            let* () = Lwt_io.chars_to_file output (Lwt_stream.of_string x) in
            Lwt.return_ok ()
        | None -> Lwt.return_error (`Not_found (Cid.to_string hash)))
    | Ok None -> Lwt.return_error `Invalid_key
    | Error e -> Lwt.return_error (e :> error)
end

module Pin = struct
  let add t hash =
    let url = url ~query:[ ("arg", Cid.to_string hash) ] t "/pin/add" in
    request url

  let rm t hash =
    let url = url ~query:[ ("arg", Cid.to_string hash) ] t "/pin/rm" in
    request url
end
