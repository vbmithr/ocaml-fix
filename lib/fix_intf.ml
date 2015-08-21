type msgname =
  | Heartbeat
  | TestRequest
  | ResendRequest
  | Reject
  | SequenceReset
  | Logout
  | Logon

type tag =
  | BeginString [@value 8]
  | BodyLength [@value 9]
  | CheckSum [@value 10]
  | MsgSeqNum [@value 34]
  | MsgType [@value 35]
  | RefSeqNum [@value 45]
  | SenderCompId [@value 49]
  | SendingTime [@value 52]
  | Symbol [@value 55]
  | TargetCompId [@value 56]
  | Text [@value 58]
  | RawData [@value 96]
  | EncryptMethod [@value 98]
  | HeartBtInt [@value 108]
  | TestReqID [@value 112]
  | ResetSeqNumFlag [@value 141]
  | NoRelatedSym [@value 146]
  | MDReqId [@value 262]
  | SubscriptionRequestType [@value 263]
  | MarketDepth [@value 264]
  | MDUpdateType [@value 265]
  | NoMDEntryTypes [@value 267]
  | MDEntryType [@value 269]
  | RefTagID [@value 371]
  | RefMsgType [@value 372]
  | Username [@value 553]
  | Password [@value 554]
      [@@deriving show, enum]
