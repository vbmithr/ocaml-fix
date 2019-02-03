open Sexplib

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end

module UTCTimestamp : sig
  val parse : string -> (Ptime.t option, Ptime.t option Tyre.error) result
  val parse_opt : string -> Ptime.t option
  val parse_exn : string -> Ptime.t
  val pp : Format.formatter -> Ptime.t -> unit
end

module YesOrNo : sig
  val parse : string -> bool option
  val parse_exn : string -> bool
  val print : bool -> string
  val pp : Format.formatter -> bool -> unit
end

module HandlInst : sig
  type t =
    | Private
    | Public
    | Manual

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module OrdStatus : sig
  type t =
    | New

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module OrdType : sig
  type t =
    | Market

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
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

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module SubscriptionRequestType : sig
  type t =
    | Snapshot
    | Subscribe
    | Unsubscribe

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module MdUpdateType : sig
  type t =
    | Full
    | Incremental

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module MdEntryType : sig
  type t =
    | Bid
    | Offer
    | Trade

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module Side : sig
  type t =
    | Buy
    | Sell

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module TimeInForce : sig
  type t =
    | Session
    | Good_till_cancel
    | At_the_opening

  val parse : string -> t option
  val parse_exn : string -> t
  val print : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
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

  val parse : string -> t option
  val parse_exn : string -> t
  val pp : Format.formatter -> t -> unit
  val print : t -> string
end

module MsgType : sig
  type t =
    | Heartbeat
    | TestRequest
    | ResendRequest
    | Reject
    | SequenceReset
    | Logout
    | Logon
    | NewOrderSingle
    | MarketDataRequest
  [@@deriving sexp]

  val parse : string -> t option
  val parse_exn : string -> t
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
  val print : t -> string
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

  val parse : string -> t option
  val parse_exn : string -> t
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
  val print : t -> string
end
