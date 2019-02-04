open Rresult

module Fixtypes : module type of Fixtypes
module Field : module type of Field

type t = {
  version : Fixtypes.Version.t ;
  typ : Fixtypes.MsgType.t ;
  sid : string ;
  tid : string ;
  seqnum : int ;
  ts : Ptime.t option ;
  fields : Field.Set.t ;
  groups : (Field.t * Field.field list list) option ;
} [@@deriving sexp]

val pp : Format.formatter -> t -> unit
val create :
  ?version:Fixtypes.Version.t ->
  ?ts:Ptime.t ->
  ?sid:string ->
  ?tid:string ->
  ?seqnum:int ->
  ?fields:Field.t list ->
  ?groups:(Field.t * Field.t list list) ->
  Fixtypes.MsgType.t -> t

val heartbeat :
  ?version:Fixtypes.Version.t ->
  ?ts:Ptime.t ->
  ?sid:string ->
  ?tid:string ->
  ?seqnum:int ->
  ?testReqID:string -> unit -> t

val to_bytes : ?buf:Buffer.t -> t -> string
val of_fields : Field.t list -> (t, R.msg) result
val read : ?pos:int -> ?len:int -> string -> (t, R.msg) result
