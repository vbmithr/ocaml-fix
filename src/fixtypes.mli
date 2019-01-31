module HandlInst : sig
  type t =
    | Private
    | Public
    | Manual

  val parse : string -> t option
  val print : t -> string
end

module OrdStatus : sig
  type t =
    | New

  val parse : string -> t option
  val print : t -> string
end

module OrdType : sig
  type t =
    | Market

  val parse : string -> t option
  val print : t -> string
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
  val print : t -> string
end

module SubscriptionRequestType : sig
  type t =
    | Snapshot
    | Subscribe
    | Unsubscribe

  val parse : string -> t option
  val print : t -> string
end

module MdUpdateType : sig
  type t =
    | Full
    | Incremental

  val parse : string -> t option
  val print : t -> string
end

module MdEntryType : sig
  type t =
    | Bid
    | Offer
    | Trade

  val parse : string -> t option
  val print : t -> string
end

module Side : sig
  type t =
    | Buy
    | Sell

  val parse : string -> t option
  val print : t -> string
end

module TimeInForce : sig
  type t =
    | Session
    | Good_till_cancel
    | At_the_opening

  val parse : string -> t option
  val print : t -> string
end

module Version : sig
  type t

  val v40 : t
  val v41 : t
  val v42 : t
  val v43 : t
  val v44 : t
  val v5  : t

  val parse : string -> t option
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

  val parse_exn : string -> t
  val pp : Format.formatter -> t -> unit
end
