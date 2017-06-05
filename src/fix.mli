module HandlInst : sig
  type t =
    | Private
    | Public
    | Manual

  val parse : string -> t
  val print : t -> string
end

module OrdStatus : sig
  type t =
    | New

  val parse : string -> t
  val print : t -> string
end

module OrdType : sig
  type t =
    | Market

  val parse : string -> t
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

  val parse : string -> t
  val print : t -> string
end

module SubscriptionRequestType : sig
  type t =
    | Snapshot
    | Subscribe
    | Unsubscribe

  val parse : string -> t
  val print : t -> string
end

module MdUpdateType : sig
  type t =
    | Full
    | Incremental

  val parse : string -> t
  val print : t -> string
end

module MdEntryType : sig
  type t =
    | Bid
    | Offer
    | Trade

  val parse : string -> t
  val print : t -> string
end

module Side : sig
  type t =
    | Buy
    | Sell

  val parse : string -> t
  val print : t -> string
end

module TimeInForce : sig
  type t =
    | Session
    | Good_till_cancel
    | At_the_opening

  val parse : string -> t
  val print : t -> string
end

module Version : sig
  type t =
    | FIX of int * int
    | FIXT of int * int

  val parse : string -> t
  val pp : Format.formatter -> t -> unit
  val print : t -> string
end

module Field : sig
  type t =
    | Account of string
    | BeginSeqNo of int
    | BeginString of Version.t
    | BodyLength of int
    | CheckSum of string
    | ClOrdID of string
    | EndSeqNo of int
    | HandlInst of HandlInst.t
    | MsgSeqNum of int
    | MsgType of string
    | NewSeqNo of int
    | OrderID of string
    | OrderQty of float
    | OrdStatus of OrdStatus.t
    | OrdType of OrdType.t
    | Price of float
    | RefSeqNum of int
    | SenderCompID of string
    | SendingTime of Ptime.t
    | Side of Side.t
    | Symbol of string
    | TargetCompID of string
    | Text of string
    | TimeInForce of TimeInForce.t
    | RawData of string
    | EncryptMethod of EncryptMethod.t
    | HeartBtInt of int
    | TestReqID of string
    | ResetSeqNumFlag of bool
    | NoRelatedSym of int
    | MDReqID of string
    | SubscriptionRequestType of SubscriptionRequestType.t
    | MarketDepth of int
    | MDUpdateType of MdUpdateType.t
    | NoMDEntryTypes of int
    | MDEntryType of MdEntryType.t
    | RefTagID of int
    | RefMsgType of string
    | Username of string
    | Password of string
    | TradeRequestID of string
    | DefaultApplVerID of string

    | Unknown of int * string

  val parse : string -> t
  val print : t -> string
end

module Message : sig
  type t =
    | Heartbeat of { testReqID : string option }
    | TestRequest of { testReqID : string }
    | ResendRequest of { beginSeqNo : int ; endSeqNo : int }
    | Reject of { refSeqNum : int }
    | SequenceReset of { newSeqNo : int }
    | Logout
    | Logon of { encryptMethod : EncryptMethod.t ; defaultApplVerID : string }
    | NewOrderSingle of { clOrdID : string }
    | MarketDataRequest

    | Unknown of { msgType : string ; fields : Field.t list }

  (* val heartbeat : ?testReqID:string -> unit -> t *)
  (* val test_request : testReqID:string -> t *)
  (* val resend_request : beginSeqNo:int -> endSeqNo:int -> t *)

  (* val reject : refSeqNum:int -> t *)

  (* val sequence_reset : newSeqNo:int -> t *)
  (* val logout : t *)
  (* val logon : encryptMethod:EncryptMethod.t -> defaultApplVerID:string -> t *)
  (* val new_order_single : clOrdID:string -> t *)
  (* val market_data_request : t *)

  (* val unknown : msgType:string -> fields:Field.t list -> t *)
end

val to_bytes : ?buf:Buffer.t -> version:Version.t -> Message.t -> string
val read : ?pos:int -> ?len:int -> string -> Message.t


