open Rresult

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
  type t =
    | FIX of int * int
    | FIXT of int * int

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
end

module Field : sig
  type _ typ =
    | Account                 : string typ
    | BeginSeqNo              : int typ
    | BeginString             : Version.t typ
    | BodyLength              : int typ
    | CheckSum                : string typ
    | ClOrdID                 : string typ
    | EndSeqNo                : int typ
    | HandlInst               : HandlInst.t typ
    | MsgSeqNum               : int typ
    | MsgType                 : MsgType.t typ
    | NewSeqNo                : int typ
    | OrderID                 : string typ
    | OrderQty                : float typ
    | OrdStatus               : OrdStatus.t typ
    | OrdType                 : OrdType.t typ
    | Price                   : float typ
    | RefSeqNum               : int typ
    | SenderCompID            : string typ
    | SendingTime             : Ptime.t typ
    | Side                    : Side.t typ
    | Symbol                  : string typ
    | TargetCompID            : string typ
    | Text                    : string typ
    | TimeInForce             : TimeInForce.t typ
    | RawData                 : string typ
    | EncryptMethod           : EncryptMethod.t typ
    | HeartBtInt              : int typ
    | TestReqID               : string typ
    | ResetSeqNumFlag         : bool typ
    | NoRelatedSym            : int typ
    | MDReqID                 : string typ
    | SubscriptionRequestType : SubscriptionRequestType.t typ
    | MarketDepth             : int typ
    | MDUpdateType            : MdUpdateType.t typ
    | NoMDEntryTypes          : int typ
    | MDEntryType             : MdEntryType.t typ
    | RefTagID                : int typ
    | RefMsgType              : MsgType.t typ
    | Username                : string typ
    | Password                : string typ
    | TradeRequestID          : string typ
    | DefaultApplVerID        : string typ

  type t

  val find : 'a typ -> t -> 'a option
  val find_list : 'a typ -> t list -> 'a option
end

type t = {
  typ : MsgType.t ;
  fields : Field.t list ;
} [@@deriving sexp]

val pp : Format.formatter -> t -> unit

val to_bytes : ?buf:Buffer.t -> version:Version.t -> t -> string
val read : ?pos:int -> ?len:int -> string -> (t, R.msg) result


