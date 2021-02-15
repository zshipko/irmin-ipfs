open Lwt.Syntax

module Hash = struct
  type t = Hash : string -> t

  let to_string (Hash h) = h

  let of_string s = Hash s
end

let exec cmd args : (string, Error.t) result Lwt.t =
  let+ s =
    Lwt_process.pread ~stderr:(`FD_copy Unix.stdout)
      (cmd, Array.append [| cmd |] args)
  in
  let s = String.trim s in
  match Astring.String.find_sub ~sub:"Error:" s with
  | Some 0 -> Error (`Msg (Astring.String.cut ~sep:"\n" s |> Option.get |> fst))
  | _ -> Ok s

let exe = ref "ipfs"

let add filename : (Hash.t, Error.t) result Lwt.t =
  let+ x = exec !exe [| "add"; "-Q"; filename |] in
  Result.map Hash.of_string x

let fetch hash : (string, Error.t) result Lwt.t =
  exec !exe [| "cat"; Hash.to_string hash |]

let get ~output hash : (unit, Error.t) result Lwt.t =
  let+ x = exec !exe [| "get"; "-o"; output; Hash.to_string hash |] in
  Result.map (fun _ -> ()) x
