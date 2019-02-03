open Astring
open Rresult
open Sexplib.Std

open Fixtypes

type _ typ  = ..

type (_,_) eq = Eq : ('a,'a) eq

module type T = sig
  type t [@@deriving sexp]
  val t : t typ
  val pp : Format.formatter -> t -> unit
  val tag : int
  val name : string
  val eq : 'a typ -> 'b typ -> ('a, 'b) eq option
  val parse : string -> t option
end

type field =
    F : 'a typ * (module T with type t = 'a) * 'a -> field
type t = field

let create typ m v = F (typ, m, v)

let pp ppf (F (_, m, v)) =
  let module F = (val m) in
  Format.fprintf ppf "@[<v 1>%s: %a@]"
    F.name Sexplib.Sexp.pp (F.sexp_of_t v)

let print (F (_, m, v)) =
  let module F = (val m) in
  Format.asprintf "%d=%a" F.tag F.pp v

let sexp_of_field t = sexp_of_string (print t)

let parse_raw str =
  match String.cut ~sep:"=" str with
  | None -> R.error_msgf "Missing '=' in '%s'" str
  | Some (tag, value) ->
    match int_of_string_opt tag with
    | None -> R.error_msg "Tag is not an int value"
    | Some tag -> R.ok (tag, value)

module SMap = Map.Make(String)

module type FIELD = sig
  include T
  val create : t -> field
  val find : 'a typ -> field -> 'a option
  val parse : int -> string -> field option
end

let field_mods = ref SMap.empty
let register_field (module F : FIELD) =
  field_mods := SMap.add F.name (module F : FIELD) !field_mods

let field_of_sexp sexp =
  match parse_raw (string_of_sexp sexp) with
  | Error _ as e -> R.failwith_error_msg e
  | Ok (tag, v) ->
    SMap.fold begin fun _ m a ->
      let module F = (val m : FIELD) in
      match F.parse tag v with
      | None -> a
      | Some t -> Some t
    end !field_mods None |> function
    | None -> failwith "field_of_sexp"
    | Some v -> v

module Set = struct
  include Set.Make(struct
    type t = field
    let compare = Pervasives.compare
  end)

  let sexp_of_t t = sexp_of_list sexp_of_field (elements t)
  let t_of_sexp s = of_list (list_of_sexp field_of_sexp s)
end

let find :
  type a. a typ -> field -> a option = fun typ field ->
  SMap.fold begin fun _ m a ->
    let module F = (val m : FIELD) in
    match F.find typ field with
    | None -> a
    | Some aa -> Some aa
  end !field_mods None

let find_set :
  type a. a typ -> Set.t -> a option = fun typ fields ->
  Set.fold begin fun f a ->
    match find typ f with
    | None -> a
    | Some v -> Some v
  end fields None

let find_and_remove_set :
  type a. a typ -> Set.t -> (a * Set.t) option = fun typ fields ->
  Set.fold begin fun f a ->
    match find typ f with
    | None -> a
    | Some v -> Some (v, Set.remove f fields)
  end fields None

exception Removed of Set.t
let remove_set :
  type a. a typ -> Set.t -> Set.t = fun typ fields ->
  try
    Set.fold begin fun f a ->
      match find typ f with
      | None -> a
      | Some _ -> raise (Removed (Set.remove f a))
    end fields fields
  with Removed s -> s

exception Parsed_ok of t
let parse str =
  let open R.Infix in
  parse_raw str >>= fun (tag, v) ->
  try
    SMap.iter begin fun _ m ->
      let module F = (val m : FIELD) in
      match F.parse tag v with
      | None -> ()
      | Some v -> raise (Parsed_ok v)
    end !field_mods ;
    R.error_msgf "Unknown tag %d" tag
  with Parsed_ok t -> R.ok t

let parser =
  let open Angstrom in
  let lift_f s =
    let open R.Infix in
    let chk = String.fold_left (fun a c -> a + Char.to_int c) 1 s in
    parse s >>| fun t -> (t, chk mod 256) in
  lift lift_f @@ take_while1 (fun c -> c <> '\x01') <* char '\x01'

let add_to_buffer buf (F (_, m, v)) =
  let module F = (val m) in
  let open Buffer in
  add_string buf (string_of_int F.tag) ;
  add_char buf '=' ;
  add_string buf (Format.asprintf "%a" F.pp v)

module Make (T : T) = struct
  include T

  let create v = (F (T.t, (module T), v))

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    match T.parse v with
    | Some v when tag' = tag -> Some (F (T.t, (module T), v))
    | _ -> None
end

type _ typ += Account : string typ
module Account = Make(struct
    type t = string [@@deriving sexp]
    let t = Account
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 1
    let name = "Account"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | Account, Account -> Some Eq
      | _ -> None
  end)
let () = register_field (module Account)

type _ typ += BeginString : Version.t typ
module BeginString = Make(struct
    type t = Version.t [@@deriving sexp]
    let t = BeginString
    let pp = Version.pp
    let parse = Version.parse
    let tag = 8
    let name = "BeginString"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | BeginString, BeginString -> Some Eq
      | _ -> None
  end)
let () = register_field (module BeginString)

type _ typ += BodyLength : int typ
module BodyLength = Make(struct
    type t = int [@@deriving sexp]
    let t = BodyLength
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 9
    let name = "BodyLength"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | BodyLength, BodyLength -> Some Eq
      | _ -> None
  end)
let () = register_field (module BodyLength)

type _ typ += CheckSum : string typ
module CheckSum = Make(struct
    type t = string [@@deriving sexp]
    let t = CheckSum
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 10
    let name = "CheckSum"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | CheckSum, CheckSum -> Some Eq
      | _ -> None
  end)
let () = register_field (module CheckSum)

type _ typ += MsgType : Fixtypes.MsgType.t typ
module MsgType = Make(struct
    type t = Fixtypes.MsgType.t [@@deriving sexp]
    let t = MsgType
    let pp = Fixtypes.MsgType.pp
    let tag = 35
    let parse = Fixtypes.MsgType.parse
    let name = "MsgType"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | MsgType, MsgType -> Some Eq
      | _ -> None
  end)
let () = register_field (module MsgType)

type _ typ += RefMsgType : Fixtypes.MsgType.t typ
module RefMsgType = Make(struct
    type t = Fixtypes.MsgType.t [@@deriving sexp]
    let t = RefMsgType
    let pp = Fixtypes.MsgType.pp
    let tag = 372
    let parse = Fixtypes.MsgType.parse
    let name = "RefMsgType"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | RefMsgType, RefMsgType -> Some Eq
      | _ -> None
  end)
let () = register_field (module RefMsgType)

type _ typ += SendingTime : Ptime.t typ
module SendingTime = Make(struct
    type t = Ptime.t [@@deriving sexp]
    let t = SendingTime
    let pp = Fixtypes.UTCTimestamp.pp
    let parse = Fixtypes.UTCTimestamp.parse_opt
    let tag = 52
    let name = "SendingTime"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SendingTime, SendingTime -> Some Eq
      | _ -> None
  end)
let () = register_field (module SendingTime)

type _ typ += SenderCompID : string typ
module SenderCompID = Make(struct
    type t = string [@@deriving sexp]
    let t = SenderCompID
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 49
    let name = "SenderCompID"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SenderCompID, SenderCompID -> Some Eq
      | _ -> None
  end)
let () = register_field (module SenderCompID)

type _ typ += TargetCompID : string typ
module TargetCompID = Make(struct
    type t = string [@@deriving sexp]
    let t = TargetCompID
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 56
    let name = "TargetCompID"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | TargetCompID, TargetCompID -> Some Eq
      | _ -> None
  end)
let () = register_field (module TargetCompID)

type _ typ += MsgSeqNum : int typ
module MsgSeqNum = Make(struct
    type t = int [@@deriving sexp]
    let t = MsgSeqNum
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 34
    let name = "MsgSeqNum"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | MsgSeqNum, MsgSeqNum -> Some Eq
      | _ -> None
  end)
let () = register_field (module MsgSeqNum)

type _ typ += RefSeqNum : int typ
module RefSeqNum = Make(struct
    type t = int [@@deriving sexp]
    let t = RefSeqNum
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 45
    let name = "RefSeqNum"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | RefSeqNum, RefSeqNum -> Some Eq
      | _ -> None
  end)
let () = register_field (module RefSeqNum)

type _ typ += SessionRejectReason : int typ
module SessionRejectReason = Make(struct
    type t = int [@@deriving sexp]
    let t = SessionRejectReason
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 373
    let name = "SessionRejectReason"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SessionRejectReason, SessionRejectReason -> Some Eq
      | _ -> None
  end)
let () = register_field (module SessionRejectReason)

type _ typ += RawData : string typ
module RawData = Make(struct
    type t = string [@@deriving sexp]
    let t = RawData
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 96
    let name = "RawData"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | RawData, RawData -> Some Eq
      | _ -> None
  end)
let () = register_field (module RawData)

type _ typ += Username : string typ
module Username = Make(struct
    type t = string [@@deriving sexp]
    let t = Username
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 553
    let name = "Username"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | Username, Username -> Some Eq
      | _ -> None
  end)
let () = register_field (module Username)

type _ typ += Password : string typ
module Password = Make(struct
    type t = string [@@deriving sexp]
    let t = Password
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 554
    let name = "Password"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | Password, Password -> Some Eq
      | _ -> None
  end)
let () = register_field (module Password)

type _ typ += Text : string typ
module Text = Make(struct
    type t = string [@@deriving sexp]
    let t = Text
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 58
    let name = "Text"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | Text, Text -> Some Eq
      | _ -> None
  end)
let () = register_field (module Text)

type _ typ += TestReqID : string typ
module TestReqID = Make(struct
    type t = string [@@deriving sexp]
    let t = TestReqID
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 112
    let name = "TestReqID"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | TestReqID, TestReqID -> Some Eq
      | _ -> None
  end)
let () = register_field (module TestReqID)

type _ typ += HeartBtInt : int typ
module HeartBtInt = Make(struct
    type t = int [@@deriving sexp]
    let t = HeartBtInt
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 108
    let name = "HeartBtInt"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | HeartBtInt, HeartBtInt -> Some Eq
      | _ -> None
  end)
let () = register_field (module HeartBtInt)

type _ typ += BeginSeqNo : int typ
module BeginSeqNo = Make(struct
    type t = int [@@deriving sexp]
    let t = BeginSeqNo
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 7
    let name = "BeginSeqNo"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | BeginSeqNo, BeginSeqNo -> Some Eq
      | _ -> None
  end)
let () = register_field (module BeginSeqNo)

type _ typ += EndSeqNo : int typ
module EndSeqNo = Make(struct
    type t = int [@@deriving sexp]
    let t = EndSeqNo
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 16
    let name = "EndSeqNo"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | EndSeqNo, EndSeqNo -> Some Eq
      | _ -> None
  end)
let () = register_field (module EndSeqNo)

type _ typ += SecurityReqID : string typ
module SecurityReqID = Make(struct
    type t = string [@@deriving sexp]
    let t = SecurityReqID
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 320
    let name = "SecurityReqID"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SecurityReqID, SecurityReqID -> Some Eq
      | _ -> None
  end)
let () = register_field (module SecurityReqID)

type _ typ += SecurityListRequestType : SecurityListRequestType.t typ
module SecurityListRequestType = Make(struct
    type t = SecurityListRequestType.t [@@deriving sexp]
    let t = SecurityListRequestType
    let pp = SecurityListRequestType.pp
    let parse = SecurityListRequestType.parse
    let tag = 559
    let name = "SecurityListRequestType"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SecurityListRequestType, SecurityListRequestType -> Some Eq
      | _ -> None
  end)
let () = register_field (module SecurityListRequestType)

type _ typ += SecurityResponseID : string typ
module SecurityResponseID = Make(struct
    type t = string [@@deriving sexp]
    let t = SecurityResponseID
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 322
    let name = "SecurityResponseID"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SecurityResponseID, SecurityResponseID -> Some Eq
      | _ -> None
  end)
let () = register_field (module SecurityResponseID)

type _ typ += SecurityRequestResult : SecurityRequestResult.t typ
module SecurityRequestResult = Make(struct
    type t = SecurityRequestResult.t [@@deriving sexp]
    let t = SecurityRequestResult
    let pp = SecurityRequestResult.pp
    let parse = SecurityRequestResult.parse
    let tag = 560
    let name = "SecurityRequestResult"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SecurityRequestResult, SecurityRequestResult -> Some Eq
      | _ -> None
  end)
let () = register_field (module SecurityRequestResult)

type _ typ += NoRelatedSym : int typ
module NoRelatedSym = Make(struct
    type t = int [@@deriving sexp]
    let t = NoRelatedSym
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 146
    let name = "NoRelatedSym"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | NoRelatedSym, NoRelatedSym -> Some Eq
      | _ -> None
  end)
let () = register_field (module NoRelatedSym)

type _ typ += RawDataLength : int typ
module RawDataLength = Make(struct
    type t = int [@@deriving sexp]
    let t = RawDataLength
    let pp = Format.pp_print_int
    let parse = int_of_string_opt
    let tag = 95
    let name = "RawDataLength"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | RawDataLength, RawDataLength -> Some Eq
      | _ -> None
  end)
let () = register_field (module RawDataLength)

type _ typ += Symbol : string typ
module Symbol = Make(struct
    type t = string [@@deriving sexp]
    let t = Symbol
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 55
    let name = "Symbol"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | Symbol, Symbol -> Some Eq
      | _ -> None
  end)
let () = register_field (module Symbol)

type _ typ += UnderlyingSymbol : string typ
module UnderlyingSymbol = Make(struct
    type t = string [@@deriving sexp]
    let t = UnderlyingSymbol
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 311
    let name = "UnderlyingSymbol"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | UnderlyingSymbol, UnderlyingSymbol -> Some Eq
      | _ -> None
  end)
let () = register_field (module UnderlyingSymbol)

type _ typ += SecurityDesc : string typ
module SecurityDesc = Make(struct
    type t = string [@@deriving sexp]
    let t = SecurityDesc
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 107
    let name = "SecurityDesc"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SecurityDesc, SecurityDesc -> Some Eq
      | _ -> None
  end)
let () = register_field (module SecurityDesc)

type _ typ += SecurityType : SecurityType.t typ
module SecurityType = Make(struct
    type t = SecurityType.t [@@deriving sexp]
    let t = SecurityType
    let pp = SecurityType.pp
    let parse = SecurityType.parse
    let tag = 167
    let name = "SecurityType"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SecurityType, SecurityType -> Some Eq
      | _ -> None
  end)
let () = register_field (module SecurityType)

type _ typ += PutOrCall : PutOrCall.t typ
module PutOrCall = Make(struct
    type t = PutOrCall.t [@@deriving sexp]
    let t = PutOrCall
    let pp = PutOrCall.pp
    let parse = PutOrCall.parse
    let tag = 201
    let name = "PutOrCall"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | PutOrCall, PutOrCall -> Some Eq
      | _ -> None
  end)
let () = register_field (module PutOrCall)

type _ typ += StrikePrice : float typ
module StrikePrice = Make(struct
    type t = float [@@deriving sexp]
    let t = StrikePrice
    let pp = Format.pp_print_float
    let parse = float_of_string_opt
    let tag = 202
    let name = "StrikePrice"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | StrikePrice, StrikePrice -> Some Eq
      | _ -> None
  end)
let () = register_field (module StrikePrice)

type _ typ += StrikeCurrency : string typ
module StrikeCurrency = Make(struct
    type t = string [@@deriving sexp]
    let t = StrikeCurrency
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 947
    let name = "StrikeCurrency"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | StrikeCurrency, StrikeCurrency -> Some Eq
      | _ -> None
  end)
let () = register_field (module StrikeCurrency)

type _ typ += Currency : string typ
module Currency = Make(struct
    type t = string [@@deriving sexp]
    let t = Currency
    let pp = Format.pp_print_string
    let parse s = Some s
    let tag = 15
    let name = "Currency"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | Currency, Currency -> Some Eq
      | _ -> None
  end)
let () = register_field (module Currency)

