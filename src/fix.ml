open StdLabels
open Astring

module UTCTimestamp = struct
  type t = Ptime.t

  let parse_date s =
    let y = String.sub_with_range s ~first:0 ~len:4 in
    let m = String.sub_with_range s ~first:4 ~len:2 in
    let d = String.sub_with_range s ~first:6 ~len:2 in
    String.(int_of_string @@ Sub.to_string y,
            int_of_string @@ Sub.to_string m,
            int_of_string @@ Sub.to_string d)

  let parse str =
    let date = ref "" in
    let h = ref 0 in
    let m = ref 0 in
    let s = ref 0 in
    let ms = ref 0 in
    begin
      try Scanf.sscanf str "%s-%d:%d:%d" begin fun dd hh mm ss ->
          date := dd ;
          h := hh ;
          m := mm ;
          s := ss
        end
      with  _ ->
        Scanf.sscanf str "%s-%d:%d:%d.%d" begin fun dd hh mm ss mmss ->
          date := dd ;
          h := hh ;
          m := mm ;
          s := ss ;
          ms := mmss ;
        end
    end ;
    let date = parse_date !date in
    match Ptime.of_date_time (date, ((!h, !m, !s), 0)),
          Ptime.Span.(of_float_s (float_of_int !ms /. 1e3))
    with
    | Some ts, Some frac -> begin
        match Ptime.(add_span ts frac) with
        | None -> invalid_arg "UTCTimestamp.parse"
        | Some ts -> ts
      end
    | _ -> invalid_arg "UTCTimestamp.parse"

  let print t =
    let ((y, m, d), ((hh, mm, ss), _)) = Ptime.to_date_time t in
    Printf.sprintf "%d%d%d-%d:%d:%d" y m d hh mm ss
end

module HandlInst = struct
  type t =
    | Private
    | Public
    | Manual

  let parse = function
    | "1" -> Private
    | "2" -> Public
    | "3" -> Manual
    | _ -> invalid_arg "HandleInst.parse"

  let print = function
    | Private -> "1"
    | Public -> "2"
    | Manual -> "3"
end

module OrdStatus = struct
  type t =
    | New

  let parse = function
    | "0" -> New
    | _ -> invalid_arg "OrdStatus.parse"

  let print = function
    | New -> "0"
end

module OrdType = struct
  type t =
    | Market

  let parse = function
    | "1" -> Market
    | _ -> invalid_arg "OrdType.parse"

  let print = function
    | Market -> "1"
end

module EncryptMethod = struct
  type t =
    | Other
    | PKCS
    | DES
    | PKCS_DES
    | PGP_DES
    | PGP_DES_MD5
    | PEM_DES_MD5

  let parse = function
    | "0" -> Other
    | "1" -> PKCS
    | "2" -> DES
    | "3" -> PKCS_DES
    | "4" -> PGP_DES
    | "5" -> PGP_DES_MD5
    | "6" -> PEM_DES_MD5
    | _ -> invalid_arg "EncryptMethod.parse"

  let print = function
    | Other -> "0"
    | PKCS -> "1"
    | DES -> "2"
    | PKCS_DES -> "3"
    | PGP_DES -> "4"
    | PGP_DES_MD5 -> "5"
    | PEM_DES_MD5 -> "6"
end

module SubscriptionRequestType = struct
  type t =
    | Snapshot
    | Subscribe
    | Unsubscribe

  let parse = function
    | "0" -> Snapshot
    | "1" -> Subscribe
    | "2" -> Unsubscribe
    | _ -> invalid_arg "SubscriptionRequestType.parse"

  let print = function
    | Snapshot -> "0"
    | Subscribe -> "1"
    | Unsubscribe -> "2"
end

module MdUpdateType = struct
  type t =
    | Full
    | Incremental

  let parse = function
    | "0" -> Full
    | "1" -> Incremental
    | _ -> invalid_arg "MdUpdateType.parse"

  let print = function
    | Full -> "0"
    | Incremental -> "1"
end

module MdEntryType = struct
  type t =
    | Bid
    | Offer
    | Trade

  let parse = function
    | "0" -> Bid
    | "1" -> Offer
    | "2" -> Trade
    | _ -> invalid_arg "MdEntryType.parse"

  let print = function
    | Bid -> "0"
    | Offer -> "1"
    | Trade -> "2"
end

module Side = struct
  type t =
    | Buy
    | Sell

  let parse = function
    | "0" -> Buy
    | "1" -> Sell
    | _ -> invalid_arg "Side.parse"

  let print = function
    | Buy -> "0"
    | Sell -> "1"
end

module TimeInForce = struct
  type t =
    | Session
    | Good_till_cancel
    | At_the_opening

  let parse = function
    | "0" -> Session
    | "1" -> Good_till_cancel
    | "2" -> At_the_opening
    | _ -> invalid_arg "TimeInForce.parse"

  let print = function
    | Session -> "0"
    | Good_till_cancel -> "1"
    | At_the_opening -> "2"
end

module YesOrNo = struct
  type t = bool

  let parse = function
    | "Y" -> true
    | "N" -> false
    | _ -> invalid_arg "YesOrNo.parse"

  let print = function
    | true -> "Y"
    | false -> "N"
end

module Version = struct
  type t =
    | FIX of int * int
    | FIXT of int * int

  let parse s =
    match String.cuts ~sep:"." s with
    | [ "FIX" ; major ; minor ] -> FIX (int_of_string major, int_of_string minor)
    | [ "FIXT" ; major ; minor ] -> FIXT (int_of_string major, int_of_string minor)
    | _ -> invalid_arg "Header.parse_version"

  let pp ppf = function
    | FIX (major, minor) -> Format.fprintf ppf "FIX.%d.%d" major minor
    | FIXT (major, minor) -> Format.fprintf ppf "FIXT.%d.%d" major minor

  let print t = Format.asprintf "%a" pp t
end

module Field = struct
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

  let parse str =
    let tag = ref 0 in
    let value = ref "" in
    Scanf.sscanf str "%d=%s" (fun t v -> tag := t ; value := v) ;
    let tag = !tag in
    let value = !value in
    match tag with
    | 1   -> Account value
    | 7   -> BeginSeqNo (int_of_string value)
    | 8   -> BeginString (Version.parse value)
    | 9   -> BodyLength (int_of_string value)
    | 10  -> CheckSum value
    | 11  -> ClOrdID value
    | 16  -> EndSeqNo (int_of_string value)
    | 21  -> HandlInst (HandlInst.parse value)
    | 34  -> MsgSeqNum (int_of_string value)
    | 35  -> MsgType value
    | 46  -> NewSeqNo (int_of_string value)
    | 37  -> OrderID value
    | 38  -> OrderQty (float_of_string value)
    | 39  -> OrdStatus (OrdStatus.parse value)
    | 40  -> OrdType (OrdType.parse value)
    | 44  -> Price (float_of_string value)
    | 45  -> RefSeqNum (int_of_string value)
    | 49  -> SenderCompID value
    | 52  -> SendingTime (UTCTimestamp.parse value)
    | 54  -> Side (Side.parse value)
    | 55  -> Symbol value
    | 56  -> TargetCompID value
    | 58  -> Text value
    | 59  -> TimeInForce (TimeInForce.parse value)
    | 96  -> RawData value
    | 98  -> EncryptMethod (EncryptMethod.parse value)
    | 108 -> HeartBtInt (int_of_string value)
    | 112 -> TestReqID value
    | 141 -> ResetSeqNumFlag (YesOrNo.parse value)
    | 146 -> NoRelatedSym (int_of_string value)
    | 262 -> MDReqID value
    | 263 -> SubscriptionRequestType (SubscriptionRequestType.parse value)
    | 264 -> MarketDepth (int_of_string value)
    | 265 -> MDUpdateType (MdUpdateType.parse value)
    | 267 -> NoMDEntryTypes (int_of_string value)
    | 269 -> MDEntryType (MdEntryType.parse value)
    | 371 -> RefTagID (int_of_string value)
    | 372 -> RefMsgType value
    | 553 -> Username value
    | 554 -> Password value
    | 568 -> TradeRequestID value
    | 1137 -> DefaultApplVerID value
    | i   -> Unknown (i, "")

  let to_string ~tag ~value =
    (string_of_int tag) ^ "=" ^ value ^ "\001"

  let add_to_buffer buf ~tag ~value =
    let open Buffer in
    add_string buf (string_of_int tag) ;
    add_char buf '=' ;
    add_string buf value ;
    add_char buf '\001'

  let print = function
    | Account value                 -> to_string 1 value
    | BeginSeqNo value              -> to_string 7 (string_of_int value)
    | BeginString value             -> to_string 8 (Version.print value)
    | BodyLength value              -> to_string 9 (string_of_int value)
    | CheckSum value                -> to_string 10 value
    | ClOrdID value                 -> to_string 11 value
    | EndSeqNo value                -> to_string 16 (string_of_int value)
    | HandlInst value               -> to_string 21 (HandlInst.print value)
    | MsgSeqNum value               -> to_string 34 (string_of_int value)
    | MsgType value                 -> to_string 35 value
    | NewSeqNo value                -> to_string 36 (string_of_int value)
    | OrderID value                 -> to_string 37 value
    | OrderQty value                -> to_string 38 (string_of_float value)
    | OrdStatus value               -> to_string 39 (OrdStatus.print value)
    | OrdType value                 -> to_string 40 (OrdType.print value)
    | Price value                   -> to_string 44 (string_of_float value)
    | RefSeqNum value               -> to_string 45 (string_of_int value)
    | SenderCompID value            -> to_string 49 value
    | SendingTime value             -> to_string 52 (UTCTimestamp.print value)
    | Side value                    -> to_string 54 (Side.print value)
    | Symbol value                  -> to_string 55 value
    | TargetCompID value            -> to_string 56 value
    | Text value                    -> to_string 58 value
    | TimeInForce value             -> to_string 59 (TimeInForce.print value)
    | RawData value                 -> to_string 96 value
    | EncryptMethod value           -> to_string 98 (EncryptMethod.print value)
    | HeartBtInt value              -> to_string 108 (string_of_int value)
    | TestReqID value               -> to_string 112 value
    | ResetSeqNumFlag value         -> to_string 141 (YesOrNo.print value)
    | NoRelatedSym value            -> to_string 146 (string_of_int value)
    | MDReqID value                 -> to_string 262 value
    | SubscriptionRequestType value -> to_string 263 (SubscriptionRequestType.print value)
    | MarketDepth value             -> to_string 264 (string_of_int value)
    | MDUpdateType value            -> to_string 265 (MdUpdateType.print value)
    | NoMDEntryTypes value          -> to_string 267 (string_of_int value)
    | MDEntryType value             -> to_string 269 (MdEntryType.print value)
    | RefTagID value                -> to_string 371 (string_of_int value)
    | RefMsgType value              -> to_string 372 value
    | Username value                -> to_string 553 value
    | Password value                -> to_string 554 value
    | TradeRequestID value          -> to_string 568 value
    | DefaultApplVerID value        -> to_string 1137 value
    | Unknown (tag, value)          -> to_string tag value

  let add_to_buffer buf = function
    | Account value                 -> add_to_buffer buf 1 value
    | BeginSeqNo value              -> add_to_buffer buf 7 (string_of_int value)
    | BeginString value             -> add_to_buffer buf 8 (Version.print value)
    | BodyLength value              -> add_to_buffer buf 9 (string_of_int value)
    | CheckSum value                -> add_to_buffer buf 10 value
    | ClOrdID value                 -> add_to_buffer buf 11 value
    | EndSeqNo value                -> add_to_buffer buf 16 (string_of_int value)
    | HandlInst value               -> add_to_buffer buf 21 (HandlInst.print value)
    | MsgSeqNum value               -> add_to_buffer buf 34 (string_of_int value)
    | MsgType value                 -> add_to_buffer buf 35 value
    | NewSeqNo value                -> add_to_buffer buf 36 (string_of_int value)
    | OrderID value                 -> add_to_buffer buf 37 value
    | OrderQty value                -> add_to_buffer buf 38 (string_of_float value)
    | OrdStatus value               -> add_to_buffer buf 39 (OrdStatus.print value)
    | OrdType value                 -> add_to_buffer buf 40 (OrdType.print value)
    | Price value                   -> add_to_buffer buf 44 (string_of_float value)
    | RefSeqNum value               -> add_to_buffer buf 45 (string_of_int value)
    | SenderCompID value            -> add_to_buffer buf 49 value
    | SendingTime value             -> add_to_buffer buf 52 (UTCTimestamp.print value)
    | Side value                    -> add_to_buffer buf 54 (Side.print value)
    | Symbol value                  -> add_to_buffer buf 55 value
    | TargetCompID value            -> add_to_buffer buf 56 value
    | Text value                    -> add_to_buffer buf 58 value
    | TimeInForce value             -> add_to_buffer buf 59 (TimeInForce.print value)
    | RawData value                 -> add_to_buffer buf 96 value
    | EncryptMethod value           -> add_to_buffer buf 98 (EncryptMethod.print value)
    | HeartBtInt value              -> add_to_buffer buf 108 (string_of_int value)
    | TestReqID value               -> add_to_buffer buf 112 value
    | ResetSeqNumFlag value         -> add_to_buffer buf 141 (YesOrNo.print value)
    | NoRelatedSym value            -> add_to_buffer buf 146 (string_of_int value)
    | MDReqID value                 -> add_to_buffer buf 262 value
    | SubscriptionRequestType value -> add_to_buffer buf 263 (SubscriptionRequestType.print value)
    | MarketDepth value             -> add_to_buffer buf 264 (string_of_int value)
    | MDUpdateType value            -> add_to_buffer buf 265 (MdUpdateType.print value)
    | NoMDEntryTypes value          -> add_to_buffer buf 267 (string_of_int value)
    | MDEntryType value             -> add_to_buffer buf 269 (MdEntryType.print value)
    | RefTagID value                -> add_to_buffer buf 371 (string_of_int value)
    | RefMsgType value              -> add_to_buffer buf 372 value
    | Username value                -> add_to_buffer buf 553 value
    | Password value                -> add_to_buffer buf 554 value
    | TradeRequestID value          -> add_to_buffer buf 568 value
    | DefaultApplVerID value        -> add_to_buffer buf 1137 value
    | Unknown (tag, value)          -> add_to_buffer buf tag value
end

module Message = struct
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

  (* TODO: Implement. *)
  let of_fields = function
    | Field.BeginString version ::
      Field.BodyLength len ::
      Field.MsgType msgType :: fields ->
        Unknown { msgType ; fields }
    | _ -> invalid_arg "Message.of_fields"

  let to_fields = function
    | Heartbeat { testReqID } -> begin
        match testReqID with
        | None -> []
        | Some id -> [Field.TestReqID id]
      end
    | TestRequest { testReqID } -> [Field.TestReqID testReqID]
    | ResendRequest { beginSeqNo ; endSeqNo } -> [
        Field.BeginSeqNo beginSeqNo ;
        Field.EndSeqNo endSeqNo ;
      ]
    | Reject { refSeqNum } -> [Field.RefSeqNum refSeqNum]
    | SequenceReset { newSeqNo } -> [Field.NewSeqNo newSeqNo]
    | Logout -> []
    | Logon { encryptMethod ; defaultApplVerID } ->
        Field.[EncryptMethod encryptMethod ;
               DefaultApplVerID defaultApplVerID]
    | NewOrderSingle { clOrdID } -> [Field.ClOrdID clOrdID]
    | MarketDataRequest -> []
    | Unknown { fields } -> fields

  let to_msgtype = function
    | Heartbeat _ -> "0"
    | TestRequest _ -> "1"
    | ResendRequest _ -> "2"
    | Reject _ -> "3"
    | SequenceReset _ -> "4"
    | Logout -> "5"
    | Logon _ -> "A"
    | NewOrderSingle _ -> "D"
    | MarketDataRequest -> "V"
    | Unknown { msgType } -> msgType
end

let to_bytes ?(buf = Buffer.create 128) ~version msg =
  let add_field buf tag value =
    Buffer.add_string buf tag;
    Buffer.add_char buf '=';
    Buffer.add_string buf value;
    Buffer.add_char buf '\001'
  in
  let fields = Field.MsgType (Message.to_msgtype msg) :: Message.to_fields msg in
  List.iter fields ~f:begin fun f ->
    Field.add_to_buffer buf f
  end ;
  let fieldslen = Buffer.length buf in
  let fields = Buffer.contents buf in
  Buffer.clear buf ;
  add_field buf "8" @@ Version.print version ;
  add_field buf "9" @@ String.of_int fieldslen ;
  Buffer.add_string buf fields ;
  let sum = String.fold_left (fun a c -> a + Char.to_int c) 0 @@ Buffer.contents buf in
  add_field buf "10" @@ Printf.sprintf "%03d" (sum mod 256);
  Buffer.contents buf

(* Compute checksum on all but the last field *)
let compute_chksum fields =
  let global, local =
    List.fold_left fields ~init:(0, 0) ~f:begin fun (global, local) sub ->
      let count = String.Sub.fold_left (fun a c -> a + Char.to_int c) 1 sub in
      global + count, count
    end in
  (global - local) mod 256

let read ?pos ?len buf =
  let buf = String.sub_with_range ?first:pos ?len buf in
  let fields = String.Sub.cuts ~sep:(String.Sub.of_char '\001') buf in
  let computed_chksum = compute_chksum fields in
  let fields = List.map fields ~f:(fun f -> Field.parse (String.Sub.to_string f)) in
  match List.rev fields with
  | CheckSum chksum :: _ ->
      if computed_chksum <> (int_of_string chksum) then
        failwith "Fix.read: bad checksum"
      else Message.of_fields fields
  | _ -> invalid_arg "Fix.read"
