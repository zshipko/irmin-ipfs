module Cid : sig
  type t = [ `Cid of string | `Path of t * string ]

  val of_string : string -> t
  val to_string : t -> string
end

type t
type error = [ `Msg of string | `Not_found of string | `Invalid_key ]
type link = { name : string; hash : Cid.t; size : int }

val v : uri:Uri.t -> t
val default : t ref
val uri : t -> Uri.t
val hash : t -> string -> (Cid.t, error) result Lwt.t
val hash' : t -> string -> Cid.t
val add_file : t -> filename:string -> (Cid.t, error) result Lwt.t
val add : t -> ?name:string -> string -> (Cid.t, error) result Lwt.t
val cat : t -> Cid.t -> (string, error) result Lwt.t
val download : t -> output:string -> Cid.t -> (unit, error) result Lwt.t
val ls : t -> Cid.t -> (link list, error) result Lwt.t

module Daemon : sig
  type t

  val start : ?wait:float -> unit -> t
  val stop : t -> unit
  val is_running : unit -> bool
end

module Pin : sig
  val add : t -> Cid.t -> (unit, error) result Lwt.t
  val rm : t -> Cid.t -> (unit, error) result Lwt.t
end
