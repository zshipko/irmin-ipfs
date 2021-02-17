module Conn : sig
  module type S = sig
    val ipfs : Ipfs.t
  end

  module Default : S
end

module Make : functor
  (Conn : Conn.S)
  (M : Irmin.Metadata.S)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  ->
  Irmin.S
    with type metadata = M.t
     and type contents = C.t
     and type key = P.t
     and type step = P.step
     and type branch = B.t
     and type hash = Ipfs.Cid.t

module KV (Conn : Conn.S) (C : Irmin.Contents.S) :
  Irmin.S
    with type metadata = unit
     and type contents = C.t
     and type key = Irmin.Path.String_list.t
     and type step = string
     and type branch = Irmin.Branch.String.t
     and type hash = Ipfs.Cid.t

module Default :
  Irmin.S
    with type metadata = unit
     and type contents = string
     and type key = Irmin.Path.String_list.t
     and type step = string
     and type branch = Irmin.Branch.String.t
     and type hash = Ipfs.Cid.t

val config : root:string -> Irmin.config
