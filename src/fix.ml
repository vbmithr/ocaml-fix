open Astring

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
    | MarketDataRequest
    | Unknown of string

  let to_string = function
    | Heartbeat -> "0"
    | TestRequest -> "1"
    | ResendRequest -> "2"
    | Reject -> "3"
    | SequenceReset -> "4"
    | Logout -> "5"
    | Logon -> "A"
    | NewOrderSingle -> "D"
    | MarketDataRequest -> "V"
    | Unknown s -> s

  let of_string = function
    | "0" -> Heartbeat
    | "1" -> TestRequest
    | "2" -> ResendRequest
    | "3" -> Reject
    | "4" -> SequenceReset
    | "5" -> Logout
    | "A" -> Logon
    | "D" -> NewOrderSingle
    | "V" -> MarketDataRequest
    | s   -> Unknown s
end

module Tag = struct
  module T = struct
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

    let of_enum = function
      | 1 -> Account
      | 7   -> BeginSeqNo
      | 8   -> BeginString
      | 9   -> BodyLength
      | 10  -> CheckSum
      | 11  -> ClOrdID
      | 16  -> EndSeqNo
      | 21  -> HandlInst
      | 34  -> MsgSeqNum
      | 35  -> MsgType
      | 37  -> OrderID
      | 38  -> OrderQty
      | 39  -> OrdStatus
      | 40  -> OrdType
      | 44  -> Price
      | 45  -> RefSeqNum
      | 49  -> SenderCompID
      | 52  -> SendingTime
      | 54  -> Side
      | 55  -> Symbol
      | 56  -> TargetCompID
      | 58  -> Text
      | 59  -> TimeInForce
      | 96  -> RawData
      | 98  -> EncryptMethod
      | 108 -> HeartBtInt
      | 112 -> TestReqID
      | 141 -> ResetSeqNumFlag
      | 146 -> NoRelatedSym
      | 262 -> MDReqID
      | 263 -> SubscriptionRequestType
      | 264 -> MarketDepth
      | 265 -> MDUpdateType
      | 267 -> NoMDEntryTypes
      | 269 -> MDEntryType
      | 371 -> RefTagID
      | 372 -> RefMsgType
      | 553 -> Username
      | 554 -> Password
      | 568 -> TradeRequestID
      | i   -> Unknown i

    let to_enum = function
      | Account                 -> 1
      | BeginSeqNo              -> 7
      | BeginString             -> 8
      | BodyLength              -> 9
      | CheckSum                -> 10
      | ClOrdID                 -> 11
      | EndSeqNo                -> 16
      | HandlInst               -> 21
      | MsgSeqNum               -> 34
      | MsgType                 -> 35
      | OrderID                 -> 37
      | OrderQty                -> 38
      | OrdStatus               -> 39
      | OrdType                 -> 40
      | Price                   -> 44
      | RefSeqNum               -> 45
      | SenderCompID            -> 49
      | SendingTime             -> 52
      | Side                    -> 54
      | Symbol                  -> 55
      | TargetCompID            -> 56
      | Text                    -> 58
      | TimeInForce             -> 59
      | RawData                 -> 96
      | EncryptMethod           -> 98
      | HeartBtInt              -> 108
      | TestReqID               -> 112
      | ResetSeqNumFlag         -> 141
      | NoRelatedSym            -> 146
      | MDReqID                 -> 262
      | SubscriptionRequestType -> 263
      | MarketDepth             -> 264
      | MDUpdateType            -> 265
      | NoMDEntryTypes          -> 267
      | MDEntryType             -> 269
      | RefTagID                -> 371
      | RefMsgType              -> 372
      | Username                -> 553
      | Password                -> 554
      | TradeRequestID          -> 568
      | Unknown i               -> i

    let compare = Pervasives.compare
  end
  include T
  module Map = Map.Make(T)
end

module Factory = struct
  type t = {
    major : int ;
    minor : int ;
    sendercompid : string ;
    targetcompid : string ;
    mutable seq : int
  }

  let seq { seq } = seq

  let create ?(seq=0) ?(major=4) ?(minor=2) ~sendercompid ~targetcompid () =
    { major ; minor ; sendercompid ; targetcompid ; seq }
end

type t = {
  major: int;
  minor: int;
  len: int;
  typ: MsgType.t;
  fields: string Tag.Map.t
}

let extract_opt ~msg = function
  | None -> invalid_arg @@ "extract_opt: " ^ msg
  | Some v -> v

let length_of_field tag value =
  let tag_len = Tag.to_enum tag |> float_of_int |> log10 |> int_of_float in
  2 + tag_len + String.length value

let field_of_sub ?expected s =
  let s_str = String.Sub.to_string s in
  let tag, value = String.Sub.cut ~sep:(String.Sub.of_char '=') s |> extract_opt ~msg:("field_of_sub: " ^ s_str) in
  let tag = String.Sub.to_int tag |> extract_opt ~msg:("field_of_sub" ^ s_str) in
  let tag = Tag.of_enum tag in
  match expected with
  | None -> tag, value
  | Some expected ->
      if expected <> tag then invalid_arg "field_of_sub: unexpected tag"
      else tag, value

let parse_version s = Scanf.sscanf s "FIX.%d.%d" (fun major minor -> major, minor)
let print_version ~major ~minor = Printf.sprintf "FIX.%d.%d" major minor

let create_msg ?check_len ~major ~minor ~typ ~fields () =
  let len = Tag.Map.fold begin fun tag value a ->
      a + length_of_field tag value
    end fields (length_of_field MsgType (MsgType.to_string typ)) in
  match check_len with
  | None -> { major ; minor ; len ; typ ; fields }
  | Some len' ->
      if len <> len' then
        invalid_arg @@ Printf.sprintf "create: check_len failure: given %d, computed %d" len' len
      else { major ; minor ; len ; typ ; fields }

let to_bytes { major; minor; typ; len; fields } =
  let add_field buf tag value =
    Buffer.add_string buf (Tag.to_enum tag |> String.of_int);
    Buffer.add_char buf '=';
    Buffer.add_string buf value;
    Buffer.add_char buf '\001'
  in
  let buf = Buffer.create 128 in
  add_field buf BeginString @@ print_version ~major ~minor;
  add_field buf BodyLength @@ String.of_int len;
  add_field buf MsgType @@ MsgType.to_string typ;
  Tag.Map.iter (fun tag value -> add_field buf tag value) fields;
  let sum = String.fold_left (fun a c -> a + Char.to_int c) 0 @@ Buffer.contents buf in
  add_field buf CheckSum @@ Printf.sprintf "%03d" (sum mod 256);
  Buffer.contents buf

let add_field msg tag value =
  { msg with
    len = msg.len + length_of_field tag value;
    fields = Tag.Map.add tag value msg.fields
  }

let timestring ts_float =
  let open Unix in
  let ms, _ = modf ts_float in
  let tm = ts_float |> gmtime in
  Printf.sprintf "%d%02d%02d-%02d:%02d:%02d.%03.0f"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour
    tm.tm_min tm.tm_sec (ms *. 1000.)

let create t typ fields' =
  let fields = Tag.Map.(add TargetCompID t.Factory.targetcompid (add SenderCompID t.sendercompid empty)) in
  let fields = Tag.Map.add MsgSeqNum (String.of_int t.seq) fields in
  let fields = Tag.Map.add SendingTime (timestring @@ Unix.gettimeofday ()) fields in
  let fields = ListLabels.fold_left fields' ~f:(fun a (tag, value) -> Tag.Map.add tag value a) ~init:fields in
  t.seq <- t.seq + 1;
  create_msg ~major:t.major ~minor:t.minor ~typ ~fields ()

let compute_chksum fields =
  ListLabels.fold_left fields ~init:0 ~f:(fun a sub ->
      String.Sub.fold_left (fun a c -> a + Char.to_int c) (a+1) sub
    ) mod 256

let read ?pos ?len buf =
  let buf = String.sub_with_range ?first:pos ?len buf in
  match String.Sub.cuts ~sep:(String.Sub.of_char '\001') buf with
  | version :: len :: typ :: fields ->
    let fields = List.(tl @@ rev fields) in
    let chksum' = compute_chksum @@ version :: len :: typ :: List.(tl fields) in
    let _, version = field_of_sub version ~expected:BeginString in
    let _, len = field_of_sub len ~expected:BodyLength in
    let _, typ = field_of_sub typ ~expected:MsgType in
    let major, minor = parse_version @@ String.Sub.to_string version in
    let len = String.Sub.to_string len |> int_of_string in
    let typ = String.Sub.to_string typ |> MsgType.of_string in
    let fields = ListLabels.fold_left fields ~init:Tag.Map.empty ~f:(fun a sub ->
        let tag, field = field_of_sub sub in
        Tag.Map.add tag (String.Sub.to_string field) a
      )
    in
    let chksum = Tag.Map.find CheckSum fields |> int_of_string in
    if chksum <> chksum' then invalid_arg "read_msg: bad checksum";
    let fields = Tag.Map.remove CheckSum fields in
    create_msg ~check_len:len ~major ~minor ~typ ~fields ()
  | _ -> invalid_arg "read_msg"
