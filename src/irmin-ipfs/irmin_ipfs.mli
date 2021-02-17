module Cid : Irmin.Hash.S with type t = Ipfs.Cid.t

module Make : functor
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
     and type hash = Cid.t

module KV (C : Irmin.Contents.S) :
  Irmin.S
    with type metadata = unit
     and type contents = C.t
     and type key = Irmin.Path.String_list.t
     and type step = string
     and type branch = Irmin.Branch.String.t
     and type hash = Cid.t

module Default :
  Irmin.S
    with type metadata = unit
     and type contents = string
     and type key = Irmin.Path.String_list.t
     and type step = string
     and type branch = Irmin.Branch.String.t
     and type hash = Cid.t

val config : ?uri:Uri.t -> root:string -> Irmin.config
