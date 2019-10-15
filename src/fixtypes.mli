(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult
open Ppx_sexp_conv_lib

module Yojsonable : sig
  module type S = sig
    type t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end
end

module type IOMIN = sig
  type t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val parse : string -> (t, R.msg) result
  val print : t -> string
end

module type IO = sig
  include IOMIN
  val parse_exn : string -> t
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module Make (T : IOMIN) : sig
  val parse_exn : string -> T.t
  val pp : Format.formatter -> T.t -> unit
  val pp_sexp : Format.formatter -> T.t -> unit
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span
  include Sexpable.S with type t := t
  include Yojsonable.S with type t := t
  val date_of_sexp : Sexp.t -> date
  val sexp_of_date : date -> Sexp.t
  val time_of_sexp : Sexp.t -> time
  val sexp_of_time : time -> Sexp.t

  val date_of_yojson : Yojson.Safe.t -> date Ppx_deriving_yojson_runtime.error_or
  val date_to_yojson : date -> Yojson.Safe.t
  val time_of_yojson : Yojson.Safe.t -> time Ppx_deriving_yojson_runtime.error_or
  val time_to_yojson : time -> Yojson.Safe.t
end

module Date : IO with type t := Ptime.date
module TZTimeOnly : IO with type t := Ptime.time

module UTCTimestamp : sig
  val parse : string -> (Ptime.t, R.msg) result
  val parse_exn : string -> Ptime.t
  val pp : Format.formatter -> Ptime.t -> unit
end

module YesOrNo : IO with type t := bool

module HandlInst : sig
  type t =
    | Private
    | Public
    | Manual
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module ExecTransType : sig
  type t =
    | New
    | Cancel
    | Correct
    | Status
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module OrdStatus : sig
  type t =
    | New
    | PartiallyFilled
    | Filled
    | DoneForDay
    | Canceled
    | Replaced
    | PendingCancel
    | Stopped
    | Rejected
    | Suspended
    | PendingNew
    | Calculated
    | Expired
    | AcceptedForBidding
    | PendingReplace
  [@@deriving sexp,yojson,bin_io]

  include IO with type t := t
end

module PosReqType : sig
  type t =
    | Positions
    | Trades
    | Exercises
    | Assignments
    | SettlementActivity
    | BackoutMessage
    | DeltaPositions
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module PosReqResult : sig
  type t =
    | ValidRequest
    | InvalidRequest
    | NoPositionsFound
    | NotAuthorized
    | Unsupported
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module OrdType : sig
  type t =
    | Market
    | Limit
    | Stop
    | StopLimit
    | MarketIfTouched
    | LimitIfTouched
  [@@deriving sexp,yojson,bin_io]

  include IO with type t := t
end

module EncryptMethod : sig
  type t =
    | Other
    | PKCS
    | DES
    | PKCS_DES
    | PGP_DES
    | PGP_DES_MD5
    | PEM_DES_MD5
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module SubscriptionRequestType : sig
  type t =
    | Snapshot
    | Subscribe
    | Unsubscribe
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module MDUpdateType : sig
  type t =
    | Full
    | Incremental
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module MDUpdateAction : sig
  type t =
    | New
    | Change
    | Delete
    | DeleteThru
    | DeleteFrom
    | Overlay
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module MDEntryType : sig
  type t =
    | Bid
    | Offer
    | Trade
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module Side : sig
  type t =
    | Buy
    | Sell
  [@@deriving sexp,yojson,bin_io]

  include IO with type t := t
end

module TimeInForce : sig
  type t =
    | Session
    | GoodTillCancel
    | AtTheOpening
    | ImmediateOrCancel
    | FillOrKill
    | GoodTillCrossing
    | GoodTillDate
    | AtTheClose
    | GoodThroughCrossing
    | AtCrossing
    | PostOnly (* Coinbase special *)
  [@@deriving sexp,yojson,bin_io]

  include IO with type t := t
end

module Version : sig
  type t =
    | FIX of int * int
    | FIXT of int * int
  [@@deriving sexp,yojson]

  val v40 : t
  val v41 : t
  val v42 : t
  val v43 : t
  val v44 : t
  val v5  : t

  include IO with type t := t
end

module MsgType : sig
  type t =
    | Heartbeat
    | TestRequest
    | ResendRequest
    | Reject
    | SequenceReset
    | Logout
    | IOI
    | Advertisement
    | ExecutionReport
    | OrderCancelReject
    | Logon
    | NewOrderSingle
    | NewOrderList
    | OrderCancelRequest
    | OrderCancelReplaceRequest
    | OrderStatusRequest
    | MarketDataRequest
    | MarketDataSnapshotFullRefresh
    | MarketDataIncrementalRefresh
    | MarketDataRequestReject
    | SecurityListRequest
    | SecurityList
    | DerivativeSecurityListRequest
    | OrderMassStatusRequest
    | RequestForPositions
    | PositionReport
    | UserRequest
    | UserResponse

    | NewOrderBatch
    | NewOrderBatchReject

    | OrderCancelBatchRequest
    | OrderCancelBatchReject
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module SessionRejectReason : sig
  type t =
    | InvalidTag
    | MissingTag
    | TagNotDefinedForThisMessage
    | UndefinedTag
    | TagWithoutValue
    | TagValueIncorrect
    | IncorrectData
    | DecryptionProblem
    | SignatureProblem
    | CompIDProblem
    | SendingTimeAccuracy
    | InvalidMsgType
    | XMLValidationError
    | InvalidVersion
    | Other
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module PutOrCall : sig
  type t =
    | Put
    | Call
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module SecurityListRequestType : sig
  type t =
    | Symbol
    | SecurityType
    | Product
    | TradingSessionID
    | AllSecurities
    | MarketID
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module SecurityRequestResult : sig
  type t =
    | Valid
  [@@deriving sexp,yojson]

  include IO with type t := t
end

module SecurityType : sig
  type t =
    | Future
    | Option
    | Index
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module QtyType : sig
  type t =
    | Units
    | Contracts
    | UnitsPerTime
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module ExecType : sig
  type t =
    | New
    | PartialFill
    | Fill
    | DoneForDay
    | Canceled
    | Replaced
    | PendingCancel
    | Stopped
    | Rejected
    | Suspended
    | PendingNew
    | Calculated
    | Expired
    | Restated
    | PendingReplace
    | Trade
    | TradeCorrect
    | TradeCancel
    | OrderStatus
    | TradeInClearingHold
    | TradeReleasedToClearing
    | Triggered
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module OrdRejReason : sig
  type t =
    | Broker
    | UnknownSymbol
    | ExchangeClosed
    | OrderExceedsLimit
    | TooLateToEnter
    | UnknownOrder
    | DuplicateOrder
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module UserRequestType : sig
  type t =
    | Logon
    | Logoff
    | ChangePassword
    | RequestStatus
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module UserStatus : sig
  type t =
    | LoggedIn
    | LoggedOff
    | NotRecognized
    | PasswordIncorrect
    | PasswordChanged
    | Other
    | ForcedLogout
    | SessionShutdownWarning
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module MassStatusReqType : sig
  type t =
    | Security
    | UnderlyingSecurity
    | Product
    | CFICode
    | SecurityType
    | TradingSession
    | AllOrders
    | PartyID
    | SecurityIssuer
    | UssuerOfUnderlyingSecurity
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module MiscFeeType : sig
  type t =
    | Regulatory
    | Tax
    | LocalCommission
    | ExchangeFees
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module CxlRejReason : sig
  type t =
    | TooLateToCancel
    | UnknownOrder
    | BrokerExchangeOption
    | PendingCancelOrReplace
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module CxlRejResponseTo : sig
  type t =
    | OrderCancelRequest
    | OrderReplaceRequest
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module ExecInst : sig
  type t =
    | DoNotIncrease
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module CancelOrdersOnDisconnect : sig
  type t =
    | All
    | Session
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module LastLiquidityInd : sig
  type t =
    | AddedLiquidity
    | RemovedLiquidity
    | LiquidityRoutedOut
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module TickDirection : sig
  type t =
    | PlusTick
    | ZeroPlusTick
    | MinusTick
    | ZeroMinusTick
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module PegPriceType : sig
  type t =
    | LastPeg
    | MidPricePeg
    | OpeningPeg
    | MarketPeg
    | PrimaryPeg
    | FixedPeg
    | PegToVWAP
    | TrailingStopPeg
    | PegToLimitPrice
  [@@deriving sexp,yojson]
  include IO with type t := t
end

module ContingencyType : sig
  type t =
    | OCO
    | OTO
    | OUOA
    | OUOP
  [@@deriving sexp,yojson]
  include IO with type t := t
end

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
