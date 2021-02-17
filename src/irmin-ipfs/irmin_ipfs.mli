module Conn : sig
  module type S = sig
    val ipfs : Ipfs.t
  end

  module Default : S
end

module type S = sig
  include Irmin.S

  val reconstruct_index : ?output:string -> Irmin.config -> unit

  val flush : repo -> unit
  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result
end

module Make : functor
  (Conn : Conn.S)
  (M : Irmin.Metadata.S)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  ->
  S
    with type metadata = M.t
     and type contents = C.t
     and type key = P.t
     and type step = P.step
     and type branch = B.t
     and type hash = Ipfs.Cid.t

module KV (Conn : Conn.S) (C : Irmin.Contents.S) :
  S
    with type metadata = unit
     and type contents = C.t
     and type key = Irmin.Path.String_list.t
     and type step = string
     and type branch = Irmin.Branch.String.t
     and type hash = Ipfs.Cid.t

module Default :
  S
    with type metadata = unit
     and type contents = string
     and type key = Irmin.Path.String_list.t
     and type step = string
     and type branch = Irmin.Branch.String.t
     and type hash = Ipfs.Cid.t

val config : root:string -> Irmin.config
