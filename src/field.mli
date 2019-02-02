open Rresult
open Fixtypes

type (_,_) eq = Eq : ('a,'a) eq

type _ typ = ..

module type T = sig
  type t [@@deriving sexp]
  val t : t typ
  val pp : Format.formatter -> t -> unit
  val tag : int
  val name : string
  val eq : 'a typ -> 'b typ -> ('a, 'b) eq option
  val parse : string -> t option
end

type field =
    F : 'a typ * (module T with type t = 'a) * 'a -> field [@@deriving sexp]
type t = field

val create : 'a typ -> (module T with type t = 'a) -> 'a -> field

val pp : Format.formatter -> t -> unit
val print : t -> string
val add_to_buffer : Buffer.t -> field -> unit
val parse : string -> (t option, R.msg) result

val find : 'a typ -> field -> 'a option
val find_list : 'a typ -> field list -> 'a option

module type FIELD = sig
  include T
  val create : t -> field
  val find : 'a typ -> field -> 'a option
  val parse : int -> string -> field option
end

val register_field : (module FIELD) -> unit

module Make (T : T) : FIELD with type t = T.t

type _ typ += Account : string typ
type _ typ += CheckSum : string typ
type _ typ += MsgType : Fixtypes.MsgType.t typ
type _ typ += MsgSeqNum : int typ
type _ typ += SenderCompID : string typ
type _ typ += TargetCompID : string typ
type _ typ += Username : string typ
type _ typ += Password : string typ
type _ typ += Text : string typ
type _ typ += TestReqID : string typ
type _ typ += BeginSeqNo : int typ
type _ typ += EndSeqNo : int typ
type _ typ += SendingTime : Ptime.t typ

module Account : FIELD with type t := string
module CheckSum : FIELD with type t := string
module MsgType : FIELD with type t := Fixtypes.MsgType.t
module RefMsgType : FIELD with type t := Fixtypes.MsgType.t
module MsgSeqNum : FIELD with type t := int
module RefSeqNum : FIELD with type t := int
module HeartBtInt : FIELD with type t := int
module BeginSeqNo : FIELD with type t := int
module EndSeqNo : FIELD with type t := int
module SenderCompID : FIELD with type t := string
module TargetCompID : FIELD with type t := string
module RawData : FIELD with type t := string
module Username : FIELD with type t := string
module Password : FIELD with type t := string
module Text : FIELD with type t := string
module TestReqID : FIELD with type t := string
module SessionRejectReason : FIELD with type t := int
