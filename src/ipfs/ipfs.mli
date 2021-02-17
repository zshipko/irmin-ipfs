module Cid : sig
  type t = [ `Cid of string ]

  val of_string : string -> t

  val to_string : t -> string
end

type t

type error = [ `Msg of string | `Not_found of string | `Invalid_key ]

val v : url:string -> t

val default : t

val hash : t -> string -> (Cid.t, error) result Lwt.t

val hash' : t -> string -> Cid.t

val add_file : t -> filename:string -> (Cid.t, error) result Lwt.t

val add : t -> ?name:string -> string -> (Cid.t, error) result Lwt.t

val cat : t -> Cid.t -> (string, error) result Lwt.t

val get : t -> Cid.t -> (unit, error) result Lwt.t

val download : t -> output:string -> Cid.t -> (unit, error) result Lwt.t

module Encrypted : sig
  module Secret : sig
    type t = Key : string -> t

    val gen : ?len:int -> unit -> t

    val of_file : filename:string -> t Lwt.t

    val to_file : t -> filename:string -> unit Lwt.t
  end

  val add_file :
    t -> secret:Secret.t -> filename:string -> (Cid.t, error) result Lwt.t

  val add :
    t ->
    secret:Secret.t ->
    ?name:string ->
    string ->
    (Cid.t, error) result Lwt.t

  val cat : t -> secret:Secret.t -> Cid.t -> (string option, error) result Lwt.t

  val download :
    t -> secret:Secret.t -> output:string -> Cid.t -> (unit, error) result Lwt.t
end

module Pin : sig
  val add : t -> Cid.t -> (unit, error) result Lwt.t

  val rm : t -> Cid.t -> (unit, error) result Lwt.t
end
