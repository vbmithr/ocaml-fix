(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Fix
open Fixtypes

val url : Uri.t
val sandbox_url : Uri.t
val tid : string
val version : Version.t

module SelfTradePrevention : sig
  type t =
    | DecrementAndCancel
    | CancelRestingOrder
    | CancelIncomingOrder
    | CancelBothOrders
  [@@deriving sexp]
end

val logon_fields :
  ?cancel_on_disconnect:[`All | `Session] ->
  key:string ->
  secret:string ->
  passphrase:string ->
  logon_ts:Ptime.t -> Field.t list

val testreq : testreqid:string -> t
val order_status_request : Uuidm.t -> t

val new_order_market :
  ?selfTradePrevention:SelfTradePrevention.t ->
  side:Side.t -> qty:float -> symbol:string -> Uuidm.t -> t

val new_order_limit_fields :
  ?selfTradePrevention:SelfTradePrevention.t ->
  side:Side.t -> price:float -> qty:float ->
  timeInForce:TimeInForce.t -> symbol:string -> Uuidm.t -> Field.t list

val new_order_limit :
  ?selfTradePrevention:SelfTradePrevention.t ->
  side:Side.t -> price:float -> qty:float ->
  timeInForce:TimeInForce.t -> symbol:string -> Uuidm.t -> t

val new_orders_limit : Uuidm.t -> Field.t list list -> t

val new_order_stop :
  ?selfTradePrevention:SelfTradePrevention.t ->
  side:Side.t -> stopPx:float -> qty:float ->
  timeInForce:TimeInForce.t -> symbol:string -> Uuidm.t -> t

val new_order_stop_limit :
  ?selfTradePrevention:SelfTradePrevention.t ->
  side:Side.t -> price:float -> stopPx:float -> qty:float ->
  timeInForce:TimeInForce.t -> symbol:string -> Uuidm.t -> t

val cancel_order :
  orderID:[`ClOrdID of Uuidm.t | `OrderID of Uuidm.t] ->
  clOrdID:Uuidm.t -> t

val cancel_orders :
  Uuidm.t -> symbol:string -> (Uuidm.t * Uuidm.t option) list -> t

type execution_report = {
  clOrdID : Uuidm.t option ;
  orderID : Uuidm.t option ;
  symbol : string ;
  side : Fixtypes.Side.t ;
  lastQty : float option ;
  price : float ;
  orderQty : float ;
  cashOrderQty : float option ;
  transactTime : Ptime.t ;
  ordStatus : Fixtypes.OrdStatus.t ;
  ordRejReason : Fixtypes.OrdRejReason.t option ;
  tradeID : Uuidm.t ;
  taker : bool ;
} [@@deriving sexp]

val parse_execution_report : t -> execution_report

type order_cancel_reject = {
  clOrdID : Uuidm.t option ;
  orderID : Uuidm.t option ;
  origClOrderID : Uuidm.t option ;
  ordStatus : Fixtypes.OrdStatus.t option ;
  cxlRejReason : Fixtypes.CxlRejReason.t option ;
  cxlRejResponseTo : Fixtypes.CxlRejResponseTo.t ;
} [@@deriving sexp]

val parse_order_cancel_reject : t -> order_cancel_reject

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
