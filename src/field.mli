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

module Set : sig
  include Set.S with type elt := field
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t :t -> Sexplib.Sexp.t
end

val create : 'a typ -> (module T with type t = 'a) -> 'a -> field

val pp : Format.formatter -> t -> unit
val print : t -> string
val add_to_buffer : Buffer.t -> field -> unit
val parse : string -> (t, R.msg) result
val parser : (t * int, R.msg) result Angstrom.t

val find : 'a typ -> field -> 'a option
val same_kind : field -> field -> bool
val find_set : 'a typ -> Set.t -> 'a option
val find_and_remove_set : 'a typ -> Set.t -> ('a * Set.t) option
val remove_set : 'a typ -> Set.t -> Set.t

module type FIELD = sig
  include T
  val create : t -> field
  val find : 'a typ -> field -> 'a option
  val parse : int -> string -> field option
end

val register_field : (module FIELD) -> unit

module Make (T : T) : FIELD with type t = T.t

type _ typ += BeginString : Version.t typ
type _ typ += BodyLength : int typ
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
type _ typ += SecurityReqID : string typ
type _ typ += SecurityListRequestType : SecurityListRequestType.t typ
type _ typ += Currency : string typ
type _ typ += StrikeCurrency : string typ
type _ typ += NoRelatedSym : int typ
type _ typ += NoMDEntryTypes : int typ
type _ typ += NoMDEntries : int typ
type _ typ += NoPositions : int typ

module NoRelatedSym : FIELD with type t := int
module NoMDEntryTypes : FIELD with type t := int
module NoMDEntries : FIELD with type t := int
module NoPositions : FIELD with type t := int

module Account : FIELD with type t := string
module Symbol : FIELD with type t := string
module MDReqID : FIELD with type t := string
module MDEntryType : FIELD with type t := Fixtypes.MDEntryType.t
module MDUpdateType : FIELD with type t := Fixtypes.MDUpdateType.t
module MarketDepth : FIELD with type t := int
module SubscriptionRequestType : FIELD with type t := Fixtypes.SubscriptionRequestType.t
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
module SendingTime : FIELD with type t := Ptime.t
module SecurityReqID : FIELD with type t := string
module SecurityListRequestType : FIELD with type t := SecurityListRequestType.t
module StrikeCurrency : FIELD with type t := string
module Currency : FIELD with type t := string
module PosReqID : FIELD with type t := string
module PosReqType : FIELD with type t := PosReqType.t
