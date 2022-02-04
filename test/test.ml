open! Lwt.Syntax

let server = Ipfs.Daemon.start ()
let () = at_exit (fun () -> Ipfs.Daemon.stop server)

let main =
  let module Store = Irmin_ipfs.Default () in
  let config = Irmin_ipfs.config ~root:"/home/zach/devel/irmin-ipfs/tmp_a" in
  let* repo = Store.Repo.v config in
  let* master = Store.main repo in
  let* () =
    Store.set_exn master
      ~info:(fun () -> Store.Info.v ~message:"test" 0L)
      [ "a"; "b"; "c" ] "123"
  in
  let* x = Store.get master [ "a"; "b"; "c" ] in
  let* () = Store.Repo.close repo in
  Lwt_io.print x

let () =
  try Lwt_main.run main
  with Curl.CurlException (_, _, msg) -> print_endline msg
