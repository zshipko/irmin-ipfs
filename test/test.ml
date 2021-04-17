open Lwt.Syntax
module Store = Irmin_ipfs.Default

let main =
  let* server = Ipfs.Daemon.start () in
  let config = Irmin_ipfs.config ~root:"/home/zach/devel/irmin-ipfs/tmp_a" in
  let* repo = Store.Repo.v config in
  let* master = Store.master repo in
  let* () =
    Store.set_exn master ~info:(Irmin_unix.info "test") [ "a"; "b"; "c" ] "123"
  in
  let* x = Store.get master [ "a"; "b"; "c" ] in
  let* () = Store.Repo.close repo in
  let* () = Ipfs.Daemon.stop server in
  Lwt_io.print x

let () = Lwt_main.run main
