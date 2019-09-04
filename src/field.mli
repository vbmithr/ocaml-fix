(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult
open Fixtypes

val int_of_string_result : string -> (int, R.msg) result
val float_of_string_result : string -> (float, R.msg) result

type (_,_) eq = Eq : ('a,'a) eq

type _ typ = ..

module type T = sig
  type t [@@deriving sexp,yojson]
  val t : t typ
  val pp : Format.formatter -> t -> unit
  val tag : int
  val name : string
  val eq : 'a typ -> 'b typ -> ('a, 'b) eq option
  val parse : string -> (t, R.msg) result
end

type field =
    F : 'a typ * (module T with type t = 'a) * 'a -> field [@@deriving sexp]

val field_to_yojson : field -> Yojson.Safe.t
val field_of_yojson : Yojson.Safe.t -> field Ppx_deriving_yojson_runtime.error_or

type t = field [@@deriving yojson]

val equal : t -> t -> bool

module Set : sig
  include Set.S with type elt := field
  include Fixtypes.Yojsonable.S with type t := t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t :t -> Sexplib.Sexp.t

  val find_typ : 'a typ -> t -> 'a option
  val find_typ_bind : 'a typ -> t -> f:('a -> 'b option) -> 'b option
  val find_typ_map : 'a typ -> t -> f:('a -> 'b) -> 'b option
  val find_and_remove_typ : 'a typ -> t -> ('a * t) option
  val remove_typ : 'a typ -> t -> t
end

val create : 'a typ -> (module T with type t = 'a) -> 'a -> field

val sum_string : string -> int
val pp : Format.formatter -> t -> unit
val print : t -> string
val add_to_buffer : int * int -> Buffer.t -> field -> int * int
val serialize : ((unit -> unit) -> int -> int -> 'a) -> Faraday.t -> t -> 'a
val parse : string -> (t, R.msg) result
val parser : (t * int, R.msg) result Angstrom.t

val find : 'a typ -> field -> 'a option
val same_kind : field -> field -> bool

module type FIELD = sig
  include T
  val create : t -> field
  val find : 'a typ -> field -> 'a option
  val parse : int -> string -> field option
  val parse_yojson : string -> Yojson.Safe.t -> field option
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
type _ typ += NoFills : int typ
type _ typ += TotNumReports : int typ
type _ typ += NoMiscFees : int typ
type _ typ += ClOrdID : string typ
type _ typ += OrigClOrdID : string typ
type _ typ += OrderID : string typ
type _ typ += Symbol : string typ
type _ typ += TradeID : string typ
type _ typ += AggressorIndicator : bool typ
type _ typ += Side : Side.t typ
type _ typ += TransactTime : Ptime.t typ
type _ typ += OrdStatus : OrdStatus.t typ
type _ typ += LastQty : float typ
type _ typ += OrderQty : float typ
type _ typ += CashOrderQty : float typ
type _ typ += Price : float typ
type _ typ += OrdRejReason : OrdRejReason.t typ
type _ typ += CxlRejReason : CxlRejReason.t typ
type _ typ += CxlRejResponseTo : CxlRejResponseTo.t typ

type _ typ += MDEntryType : MDEntryType.t typ
type _ typ += MDEntryPx : float typ
type _ typ += MDEntrySize : float typ
type _ typ += MDEntryDate : Ptime.t typ

type _ typ += ExecInst : ExecInst.t typ
type _ typ += CancelOrdersOnDisconnect : CancelOrdersOnDisconnect.t typ

module NoRelatedSym : FIELD with type t := int
module NoMDEntryTypes : FIELD with type t := int
module NoMDEntries : FIELD with type t := int
module NoPositions : FIELD with type t := int
module NoFills : FIELD with type t := int
module TotNumReports : FIELD with type t := int
module NoMiscFees : FIELD with type t := int

module Account : FIELD with type t := string
module EncryptMethod : FIELD with type t := Fixtypes.EncryptMethod.t
module HandlInst : FIELD with type t := Fixtypes.HandlInst.t
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
module MassStatusReqID : FIELD with type t := string
module PosReqID : FIELD with type t := string
module PosReqType : FIELD with type t := PosReqType.t
module MassStatusReqType : FIELD with type t := MassStatusReqType.t
module UserRequestID : FIELD with type t := string
module UserRequestType : FIELD with type t := UserRequestType.t
module ClOrdID : FIELD with type t := string
module OrigClOrdID : FIELD with type t := string
module OrderID : FIELD with type t := string
module SecondaryOrderID : FIELD with type t := string
module Side : FIELD with type t := Side.t
module OrderQty : FIELD with type t := float
module CashOrderQty : FIELD with type t := float
module OrdType : FIELD with type t := OrdType.t
module Price : FIELD with type t := float
module StopPx : FIELD with type t := float
module TimeInForce : FIELD with type t := TimeInForce.t
module OrdStatus : FIELD with type t := OrdStatus.t
module ExecInst : FIELD with type t := ExecInst.t
module CancelOrdersOnDisconnect : FIELD with type t := CancelOrdersOnDisconnect.t

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
