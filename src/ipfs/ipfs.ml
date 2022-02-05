open Lwt.Syntax

type error = [ `Msg of string | `Not_found of string | `Invalid_key ]

module Cid = struct
  type t = [ `Cid of string | `Path of t * string ]

  let rec to_string = function
    | `Cid s -> s
    | `Path (a, s) -> to_string a ^ "/" ^ s

  let of_string s =
    if String.contains s '/' then
      let s = String.split_on_char '/' s in
      let path = List.tl s |> String.concat "/" in
      let s = List.hd s in
      `Path (`Cid s, path)
    else `Cid s
end

type t = { uri : Uri.t }

let v ~uri = { uri }
let default = ref (v ~uri:(Uri.of_string "http://127.0.0.1:5001"))
let () = Curl.global_init Curl.CURLINIT_GLOBALALL
let () = at_exit Curl.global_cleanup
let uri t = t.uri

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
        | exn ->
            Curl.cleanup req;
            raise exn)
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
    (fun () ->
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

type link = { name : string; hash : Cid.t; size : int }

let ls t hash =
  let output = Buffer.create 256 in
  let url = url t ~query:[ ("arg", Cid.to_string hash) ] "/ls" in
  let+ res = request ~output url in
  Result.map
    (fun () ->
      let json = Ezjsonm.from_string (Buffer.contents output) in
      let obj item =
        let link (x : (string * Ezjsonm.value) list) =
          try
            let hash =
              List.assoc "Hash" x |> Ezjsonm.get_string |> Cid.of_string
            in
            let name = List.assoc "Name" x |> Ezjsonm.get_string in
            let size = List.assoc "Size" x |> Ezjsonm.get_int in
            Some { name; hash; size }
          with _ -> None
        in
        let f x = Ezjsonm.get_dict x |> link in
        Ezjsonm.find item [ "Links" ]
        |> Ezjsonm.get_list f |> List.filter_map Fun.id
      in
      Ezjsonm.find json [ "Objects" ] |> Ezjsonm.get_list obj |> List.hd)
    res

let download t ~output hash : (unit, error) result Lwt.t =
  let* x = cat t hash in
  match x with
  | Ok s ->
      let* () = Lwt_io.chars_to_file output (Lwt_stream.of_string s) in
      Lwt.return_ok ()
  | Error e -> Lwt.return_error e

module Pin = struct
  let add t hash =
    let url = url ~query:[ ("arg", Cid.to_string hash) ] t "/pin/add" in
    request url

  let rm t hash =
    let url = url ~query:[ ("arg", Cid.to_string hash) ] t "/pin/rm" in
    request url
end

let devnull = Unix.openfile "/dev/null" Unix.[ O_RDWR ] 0o655
let () = at_exit (fun () -> Unix.close devnull)

module Daemon = struct
  type t = { proc : int option }

  let is_running () =
    let home = Unix.getenv "HOME" in
    let file = Filename.concat home ".ipfs/api" in
    try Sys.file_exists file with _ -> false

  let start ?(wait = 1.0) () =
    if not (is_running ()) then (
      let pid =
        Unix.create_process "ipfs"
          [| "ipfs"; "daemon"; "--init" |]
          Unix.stdin devnull devnull
      in
      let n = ref 0 in
      let () =
        while not (is_running ()) do
          if !n >= 10 then failwith "Cannot start ipfs";
          incr n;
          Unix.sleepf wait
        done
      in
      let x = { proc = Some pid } in
      Gc.finalise
        (fun _ ->
          Unix.kill pid Sys.sigint;
          Unix.kill pid Sys.sigint)
        x;
      x)
    else { proc = None }

  let stop { proc; _ } =
    match proc with
    | Some proc ->
        let () = Unix.kill proc Sys.sigint in
        let () = Unix.kill proc Sys.sigint in
        ignore (Unix.waitpid [] proc)
    | None -> ()
end
