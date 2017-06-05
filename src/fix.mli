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

    | Unknown of string

  val of_string : string -> t
  val to_string : t -> string
end

module Tag : sig
  type t =
    | Account
    | BeginSeqNo
    | BeginString
    | BodyLength
    | CheckSum
    | ClOrdID
    | EndSeqNo
    | HandlInst
    | MsgSeqNum
    | MsgType
    | OrderID
    | OrderQty
    | OrdStatus
    | OrdType
    | Price
    | RefSeqNum
    | SenderCompID
    | SendingTime
    | Side
    | Symbol
    | TargetCompID
    | Text
    | TimeInForce
    | RawData
    | EncryptMethod
    | HeartBtInt
    | TestReqID
    | ResetSeqNumFlag
    | NoRelatedSym
    | MDReqID
    | SubscriptionRequestType
    | MarketDepth
    | MDUpdateType
    | NoMDEntryTypes
    | MDEntryType
    | RefTagID
    | RefMsgType
    | Username
    | Password
    | TradeRequestID

    | Unknown of int

  val of_enum : int -> t
  val to_enum : t -> int
  val compare : t -> t -> int
  module Map : Map.S with type key = t
end

module Factory : sig
  type t

  val create :
    ?seq:int ->
    ?major:int ->
    ?minor:int ->
    sendercompid:string ->
    targetcompid:string ->
    unit -> t

  val seq : t -> int
end

type t = private {
  major: int;
  minor: int;
  len: int;
  typ: MsgType.t;
  fields: string Tag.Map.t
}

val create :
  Factory.t -> MsgType.t -> (Tag.t * string) list -> t

val add_field : t -> Tag.t -> string -> t
val read : ?pos:int -> ?len:int -> string -> t
val to_bytes : t -> string

