(lang dune 2.8)
(name irmin-ipfs)

(generate_opam_files true)

(source (github zshipko/irmin-ipfs))
(license ISC)
(authors "Zach Shipko")
(maintainers "zachshipko@gmail.com")

(package
 (name ipfs)
 (synopsis "Bindings to ipfs")
 (description "Bindings to the ipfs HTTP API")
 (depends
  (dune (>= 2.8))
  ezjsonm
  ocurl
  lwt))

(package
 (name irmin-ipfs)
 (synopsis "ipfs backend for irmin")
 (description "ipfs backend for irmin")
 (depends
  (dune (>= 2.8))
  ipfs
  irmin))


