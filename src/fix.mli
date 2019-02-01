open Rresult

module Fixtypes : module type of Fixtypes
module Field : module type of Field

type t = {
  typ : Fixtypes.MsgType.t ;
  fields : Field.t list ;
} [@@deriving sexp]

val pp : Format.formatter -> t -> unit
val create : typ:Fixtypes.MsgType.t -> fields:Field.t list -> t

val heartbeat :
  senderCompID:string ->
  targetCompID:string ->
  testReqID:string -> int -> t

val to_bytes : ?buf:Buffer.t -> version:Fixtypes.Version.t -> t -> string
val read : ?pos:int -> ?len:int -> string -> (t, R.msg) result
