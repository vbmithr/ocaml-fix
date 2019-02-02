open Rresult

module Fixtypes : module type of Fixtypes
module Field : module type of Field

type t = {
  typ : Fixtypes.MsgType.t ;
  sid : string ;
  tid : string ;
  seqnum : int ;
  ts : Ptime.t option ;
  fields : Field.field list ;
} [@@deriving sexp]

val pp : Format.formatter -> t -> unit
val create :
  ?ts:Ptime.t ->
  ?sid:string ->
  ?tid:string ->
  ?seqnum:int ->
  ?fields:Field.t list ->
  Fixtypes.MsgType.t -> t

val heartbeat :
  ?ts:Ptime.t ->
  ?sid:string ->
  ?tid:string ->
  ?seqnum:int ->
  ?testReqID:string -> unit -> t

val to_bytes : ?buf:Buffer.t -> version:Fixtypes.Version.t -> t -> string
val read : ?pos:int -> ?len:int -> string -> (t, R.msg) result
