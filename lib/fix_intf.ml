module MsgType = struct
  type t =
    | Heartbeat
    | TestRequest
    | ResendRequest
    | Reject
    | SequenceReset
    | Logout
    | Logon
    | NewOrderSingle
  [@@deriving sexp]

  let to_string = function
    | Heartbeat -> "0"
    | TestRequest -> "1"
    | ResendRequest -> "2"
    | Reject -> "3"
    | SequenceReset -> "4"
    | Logout -> "5"
    | Logon -> "A"
    | NewOrderSingle -> "D"

  let of_string = function
    | "0" -> Heartbeat
    | "1" -> TestRequest
    | "2" -> ResendRequest
    | "3" -> Reject
    | "4" -> SequenceReset
    | "5" -> Logout
    | "A" -> Logon
    | "D" -> NewOrderSingle
    | _ -> invalid_arg "msgname_of_string"
end

module Tag = struct
  module T = struct
    open Sexplib.Std
    type std =
      | BeginSeqNo [@value 7]
      | BeginString [@value 8]
      | BodyLength [@value 9]
      | CheckSum [@value 10]
      | ClOrdID [@value 11]
      | EndSeqNo [@value 16]
      | HandlInst [@value 21]
      | MsgSeqNum [@value 34]
      | MsgType [@value 35]
      | OrderID [@value 37]
      | OrderQty [@value 38]
      | OrdStatus [@value 39]
      | OrdType [@value 40]
      | Price [@value 44]
      | RefSeqNum [@value 45]
      | SenderCompID [@value 49]
      | SendingTime [@value 52]
      | Side [@value 54]
      | Symbol [@value 55]
      | TargetCompID [@value 56]
      | Text [@value 58]
      | TimeInForce [@value 59]
      | RawData [@value 96]
      | EncryptMethod [@value 98]
      | HeartBtInt [@value 108]
      | TestReqID [@value 112]
      | ResetSeqNumFlag [@value 141]
      | NoRelatedSym [@value 146]
      | MDReqID [@value 262]
      | SubscriptionRequestType [@value 263]
      | MarketDepth [@value 264]
      | MDUpdateType [@value 265]
      | NoMDEntryTypes [@value 267]
      | MDEntryType [@value 269]
      | RefTagID [@value 371]
      | RefMsgType [@value 372]
      | Username [@value 553]
      | Password [@value 554]
      | TradeRequestID [@value 568]
    [@@deriving enum, sexp]

    type t =
      | S of std
      | C of int [@@deriving sexp]

    let to_enum = function
      | S std -> std_to_enum std
      | C i -> i

    let of_enum i = match std_of_enum i with
      | Some std -> S std
      | None -> C i

    let compare = Pervasives.compare
  end
  include T
  module Map = struct
    include Map.Make(T)
    let t_of_sexp value_of_sexp = function
      | Sexplib.Sexp.List sexps ->
        ListLabels.fold_left sexps ~init:empty ~f:(fun a sexp ->
            match sexp with
            | Sexplib.Sexp.List [tag; value] -> add (t_of_sexp tag) (value_of_sexp value) a
            | _ -> invalid_arg "t_of_sexp"
          )
      | _ -> invalid_arg "t_of_sexp"
    let sexp_of_t sexp_of_value t =
      let open Sexplib in
      let t = bindings t in
      let t = ListLabels.map t ~f:(fun (key, value) -> Sexp.List [sexp_of_t key; sexp_of_value value]) in
      Sexp.List t
  end
end
