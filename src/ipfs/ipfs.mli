module Cid : sig
  type t = [ `Cid of string ]

  val of_string : string -> t

  val to_string : t -> string
end

type t

type error = [ `Msg of string | `Not_found of string | `Invalid_key ]

val v : url:string -> t

val default : t ref

val hash : t -> string -> (Cid.t, error) result Lwt.t

val hash' : t -> string -> Cid.t

val add_file : t -> filename:string -> (Cid.t, error) result Lwt.t

val add : t -> ?name:string -> string -> (Cid.t, error) result Lwt.t

val cat : t -> Cid.t -> (string, error) result Lwt.t

val get : t -> Cid.t -> (unit, error) result Lwt.t

val download : t -> output:string -> Cid.t -> (unit, error) result Lwt.t

module Pin : sig
  val add : t -> Cid.t -> (unit, error) result Lwt.t

  val rm : t -> Cid.t -> (unit, error) result Lwt.t
end
