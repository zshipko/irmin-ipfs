module Conn : sig
  module type S = sig
    val ipfs : Ipfs.t
  end

  module Default : S
end

module type S = Irmin.S

module Make : functor
  (Conn : Conn.S)
  (Schema : Irmin.Schema.S with type Hash.t = Ipfs.Cid.t)
  ->
  S
    with type Schema.Metadata.t = Schema.Metadata.t
     and type Schema.Contents.t = Schema.Contents.t
     and type Schema.Path.t = Schema.Path.t
     and type Schema.Path.step = Schema.Path.step
     and type Schema.Branch.t = Schema.Branch.t
     and type hash = Ipfs.Cid.t

module KV (Conn : Conn.S) (C : Irmin.Contents.S) :
  S
    with type Schema.Metadata.t = unit
     and type Schema.Contents.t = C.t
     and type Schema.Path.t = string list
     and type Schema.Path.step = string
     and type Schema.Branch.t = string
     and type hash = Ipfs.Cid.t

module Default () :
  S
    with type Schema.Metadata.t = unit
     and type Schema.Contents.t = string
     and type Schema.Path.t = string list
     and type Schema.Path.step = string
     and type Schema.Branch.t = string
     and type hash = Ipfs.Cid.t

val config : root:string -> Irmin.config
