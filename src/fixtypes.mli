(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Sexplib

module type IO = sig
  type t [@@deriving sexp]

  val parse : string -> t option
  val print : t -> string
  val parse_exn : string -> t
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val date_of_sexp : Sexp.t -> date
  val sexp_of_date : date -> Sexp.t
  val time_of_sexp : Sexp.t -> time
  val sexp_of_time : time -> Sexp.t
end

module Date : IO with type t := Ptime.date
module TZTimeOnly : IO with type t := Ptime.time

module UTCTimestamp : sig
  val parse : string -> (Ptime.t option, Ptime.t option Tyre.error) result
  val parse_opt : string -> Ptime.t option
  val parse_exn : string -> Ptime.t
  val pp : Format.formatter -> Ptime.t -> unit
end

module YesOrNo : IO with type t := bool

module HandlInst : sig
  type t =
    | Private
    | Public
    | Manual

  include IO with type t := t
end

module OrdStatus : sig
  type t =
    | New
    | PartiallyFilled
    | Filled
    | DoneForDay
    | Canceled

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

  include IO with type t := t
end

module PosReqResult : sig
  type t =
    | ValidRequest
    | InvalidRequest
    | NoPositionsFound
    | NotAuthorized
    | Unsupported

  include IO with type t := t
end

module OrdType : sig
  type t =
    | Market
    | Limit
    | Stop
    | StopLimit
    | MarketOnClose
    | WithOrWithout

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

  include IO with type t := t
end

module SubscriptionRequestType : sig
  type t =
    | Snapshot
    | Subscribe
    | Unsubscribe

  include IO with type t := t
end

module MDUpdateType : sig
  type t =
    | Full
    | Incremental

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

  include IO with type t := t
end

module MDEntryType : sig
  type t =
    | Bid
    | Offer
    | Trade

  include IO with type t := t
end

module Side : sig
  type t =
    | Buy
    | Sell

  include IO with type t := t
end

module TimeInForce : sig
  type t =
    | Session
    | Good_till_cancel
    | At_the_opening

  include IO with type t := t
end

module Version : sig
  type t =
    | FIX of int * int
    | FIXT of int * int
  [@@deriving sexp]

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
  [@@deriving sexp]

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
  [@@deriving sexp]

  include IO with type t := t
end

module PutOrCall : sig
  type t =
    | Put
    | Call
  [@@deriving sexp]

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
  [@@deriving sexp]

  include IO with type t := t
end

module SecurityRequestResult : sig
  type t =
    | Valid
  [@@deriving sexp]

  include IO with type t := t
end

module SecurityType : sig
  type t =
    | Future
    | Option
    | Index
  [@@deriving sexp]
  include IO with type t := t
end

module QtyType : sig
  type t =
    | Units
    | Contracts
    | UnitsPerTime
  [@@deriving sexp]
  include IO with type t := t
end

module ExecType : sig
  type t =
    | New
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
  [@@deriving sexp]
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
  [@@deriving sexp]
  include IO with type t := t
end

module UserRequestType : sig
  type t =
    | Logon
    | Logoff
    | ChangePassword
    | RequestStatus

  [@@deriving sexp]
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

  [@@deriving sexp]
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
