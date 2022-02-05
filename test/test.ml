open! Lwt.Syntax

let server = Ipfs.Daemon.start ()
let () = at_exit (fun () -> Ipfs.Daemon.stop server)

let main =
  let module Store = Irmin_ipfs.Default () in
  let config = Irmin_ipfs.config ~root:"tmp_a" in
  let* repo = Store.Repo.v config in
  let* main = Store.main repo in
  let* value =
    Store.Contents.of_hash repo
      (`Path (`Cid "QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG", "readme"))
  in
  let value = Option.get value in
  let* () =
    Store.set_exn main
      ~info:(fun () -> Store.Info.v ~message:"test" 0L)
      [ "a"; "b"; "c" ] value
  in
  let* x = Store.get main [ "a"; "b"; "c" ] in
  let* () = Store.Repo.close repo in
  Lwt_io.print x

let () =
  try Lwt_main.run main
  with Curl.CurlException (_, _, msg) -> print_endline msg
