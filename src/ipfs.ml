open Lwt.Syntax

module Hash = struct
  type t = string
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
  exec !exe [| "add"; "-Q"; filename |]

let get hash = exec !exe [| "cat"; hash |]
