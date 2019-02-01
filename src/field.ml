open Astring
open Rresult
open Sexplib.Std

open Fixtypes

type _ typ  = ..

type (_,_) eq = Eq : ('a,'a) eq

module type T = sig
  type t [@@deriving sexp]
  val pp : Format.formatter -> t -> unit
  val tag : int
  val name : string
end

type field =
    F : 'a typ * (module T with type t = 'a) * 'a -> field
type t = field

let create typ m v = F (typ, m, v)

module type FIELD = sig
  include T
  val create : t -> field
  val find : 'a typ -> field -> 'a option
  val parse : int -> string -> field option
end

module SMap = Map.Make(String)

let parse_raw str =
  match String.cut ~sep:"=" str with
  | None -> R.error_msgf "Missing '=' in '%s'" str
  | Some (tag, value) ->
    match int_of_string_opt tag with
    | None -> R.error_msg "Tag is not an int value"
    | Some tag -> R.ok (tag, value)

let field_mods = ref SMap.empty

let register_field (module F : FIELD) =
  field_mods := SMap.add F.name (module F : FIELD) !field_mods

let find :
  type a. a typ -> field -> a option = fun typ field ->
  SMap.fold begin fun _ m a ->
    let module F = (val m : FIELD) in
    match F.find typ field with
    | None -> a
    | Some aa -> Some aa
  end !field_mods None

let find_list :
  type a. a typ -> field list -> a option = fun typ fields ->
  List.fold_left begin fun a f ->
    match find typ f with
    | None -> a
    | Some v -> Some v
  end None fields

exception Parsed_ok of t
let parse str =
  let open R.Infix in
  parse_raw str >>| fun (tag, v) ->
  try
    SMap.iter begin fun _ m ->
      let module F = (val m : FIELD) in
      match F.parse tag v with
      | None -> ()
      | Some v -> raise (Parsed_ok v)
    end !field_mods ;
    None
  with Parsed_ok t -> Some t

let pp ppf (F (_, m, a)) =
  let module F = (val m) in
  Format.fprintf ppf "@[<v 1>%s: %a@]" F.name F.pp a

let print (F (typ, m, v)) =
  let module F = (val m) in
  Format.asprintf "%d=%a" F.tag F.pp v

let add_to_buffer buf (F (typ, m, v)) =
  let module F = (val m) in
  let open Buffer in
  add_string buf (string_of_int F.tag) ;
  add_char buf '=' ;
  add_string buf (Format.asprintf "%a" F.pp v)

let sexp_of_field t = sexp_of_string (print t)

let field_of_sexp sexp =
  match parse_raw (string_of_sexp sexp) with
  | Error _ as e -> R.failwith_error_msg e
  | Ok (tag, v) ->
  SMap.fold begin fun _ m a ->
    let module F = (val m : FIELD) in
    F.parse tag v
  end !field_mods None |> function
  | None -> failwith "field_of_sexp"
  | Some v -> v

type _ typ += Account : string typ

module Account = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 1
    let name = "Account"
  end

  include T

  let create v = (F (Account, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | Account, Account -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (Account, (module T), v))
    else None
end

let () = register_field (module Account)

type _ typ += BeginString : string typ

module BeginString = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 8
    let name = "BeginString"
  end

  include T

  let create v = (F (BeginString, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | BeginString, BeginString -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (BeginString, (module T), v))
    else None
end

let () = register_field (module BeginString)

type _ typ += BodyLength : string typ

module BodyLength = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 9
    let name = "BodyLength"
  end

  include T

  let create v = (F (BodyLength, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | BodyLength, BodyLength -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (BodyLength, (module T), v))
    else None
end

let () = register_field (module BodyLength)

type _ typ += CheckSum : string typ

module CheckSum = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 10
    let name = "CheckSum"
  end

  include T
  let create v = (F (CheckSum, (module T), v))
  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | CheckSum, CheckSum -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (CheckSum, (module T), v))
    else None
end

let () = register_field (module CheckSum)

type _ typ += MsgType : Fixtypes.MsgType.t typ

module MsgType = struct
  module T = struct
    type t = Fixtypes.MsgType.t [@@deriving sexp]
    let pp = Fixtypes.MsgType.pp
    let tag = 35
    let name = "MsgType"
  end

  include T
  let create v = (F (MsgType, (module T), v))
  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | MsgType, MsgType -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (MsgType, (module T), Fixtypes.MsgType.parse_exn v))
    else None
end

let () = register_field (module MsgType)

type _ typ += SendingTime : Ptime.t typ

module SendingTime = struct
  module T = struct
    type t = Ptime.t [@@deriving sexp]
    let pp = Fixtypes.UTCTimestamp.pp
    let tag = 52
    let name = "SendingTime"
  end

  include T
  let create v = (F (SendingTime, (module T), v))
  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | SendingTime, SendingTime -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (SendingTime, (module T), Fixtypes.UTCTimestamp.parse_exn v))
    else None
end

let () = register_field (module SendingTime)

type _ typ += SenderCompID : string typ

module SenderCompID = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 49
    let name = "SenderCompID"
  end

  include T

  let create v = (F (SenderCompID, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | SenderCompID, SenderCompID -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (SenderCompID, (module T), v))
    else None
end

let () = register_field (module SenderCompID)

type _ typ += TargetCompID : string typ

module TargetCompID = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 56
    let name = "TargetCompID"
  end

  include T

  let create v = (F (TargetCompID, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | TargetCompID, TargetCompID -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (TargetCompID, (module T), v))
    else None
end

let () = register_field (module TargetCompID)

type _ typ += MsgSeqNum : int typ

module MsgSeqNum = struct
  module T = struct
    type t = int [@@deriving sexp]
    let pp = Format.pp_print_int
    let tag = 34
    let name = "MsgSeqNum"
  end

  include T

  let create v = (F (MsgSeqNum, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | MsgSeqNum, MsgSeqNum -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (MsgSeqNum, (module T), int_of_string v))
    else None
end

let () = register_field (module MsgSeqNum)

type _ typ += RawData : string typ

module RawData = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 96
    let name = "RawData"
  end

  include T

  let create v = (F (RawData, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | RawData, RawData -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (RawData, (module T), v))
    else None
end

let () = register_field (module RawData)

type _ typ += Username : string typ

module Username = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 553
    let name = "Username"
  end

  include T

  let create v = (F (Username, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | Username, Username -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (Username, (module T), v))
    else None
end

let () = register_field (module Username)

type _ typ += Password : string typ

module Password = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 554
    let name = "Password"
  end

  include T

  let create v = (F (Password, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | Password, Password -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (Password, (module T), v))
    else None
end

let () = register_field (module Password)

type _ typ += Text : string typ

module Text = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 58
    let name = "Text"
  end

  include T

  let create v = (F (Text, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | Text, Text -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (Text, (module T), v))
    else None
end

let () = register_field (module Text)

type _ typ += TestReqID : string typ

module TestReqID = struct
  module T = struct
    type t = string [@@deriving sexp]
    let pp = Format.pp_print_string
    let tag = 112
    let name = "TestReqID"
  end

  include T

  let create v = (F (TestReqID, (module T), v))

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | TestReqID, TestReqID -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (TestReqID, (module T), v))
    else None
end

let () = register_field (module TestReqID)

(* type _ typ += Account                 : string typ
 * type _ typ += BeginSeqNo              : int typ
 * type _ typ += BeginString             : Version.t typ
 * type _ typ += BodyLength              : int typ
 * type _ typ += CheckSum                : string typ
 * type _ typ += ClOrdID                 : string typ
 * type _ typ += EndSeqNo                : int typ
 * type _ typ += HandlInst               : HandlInst.t typ
 * type _ typ += MsgSeqNum               : int typ
 * type _ typ += MsgType                 : MsgType.t typ
 * type _ typ += NewSeqNo                : int typ
 * type _ typ += OrderID                 : string typ
 * type _ typ += OrderQty                : float typ
 * type _ typ += OrdStatus               : OrdStatus.t typ
 * type _ typ += OrdType                 : OrdType.t typ
 * type _ typ += Price                   : float typ
 * type _ typ += RefSeqNum               : int typ
 * type _ typ += SenderCompID            : string typ
 * type _ typ += SendingTime             : Ptime.t typ
 * type _ typ += Side                    : Side.t typ
 * type _ typ += Symbol                  : string typ
 * type _ typ += TargetCompID            : string typ
 * type _ typ += Text                    : string typ
 * type _ typ += TimeInForce             : TimeInForce.t typ
 * type _ typ += RawData                 : string typ
 * type _ typ += EncryptMethod           : EncryptMethod.t typ
 * type _ typ += HeartBtInt              : int typ
 * type _ typ += TestReqID               : string typ
 * type _ typ += ResetSeqNumFlag         : bool typ
 * type _ typ += NoRelatedSym            : int typ
 * type _ typ += MDReqID                 : string typ
 * type _ typ += SubscriptionRequestType : SubscriptionRequestType.t typ
 * type _ typ += MarketDepth             : int typ
 * type _ typ += MDUpdateType            : MdUpdateType.t typ
 * type _ typ += NoMDEntryTypes          : int typ
 * type _ typ += MDEntryType             : MdEntryType.t typ
 * type _ typ += RefTagID                : int typ
 * type _ typ += RefMsgType              : MsgType.t typ
 * type _ typ += Username                : string typ
 * type _ typ += Password                : string typ
 * type _ typ += TradeRequestID          : string typ
 * type _ typ += DefaultApplVerID        : string typ *)


(* let tag_of_typ_exn (type a) (typ : a typ) =
 *   match typ with
 *   | Account                 -> 1
 *   | BeginSeqNo              -> 7
 *   | BeginString             -> 8
 *   | BodyLength              -> 9
 *   | CheckSum                -> 10
 *   | ClOrdID                 -> 11
 *   | EndSeqNo                -> 16
 *   | HandlInst               -> 21
 *   | MsgSeqNum               -> 34
 *   | MsgType                 -> 35
 *   | NewSeqNo                -> 36
 *   | OrderID                 -> 37
 *   | OrderQty                -> 38
 *   | OrdStatus               -> 39
 *   | OrdType                 -> 40
 *   | Price                   -> 44
 *   | RefSeqNum               -> 45
 *   | SenderCompID            -> 49
 *   | SendingTime             -> 52
 *   | Side                    -> 54
 *   | Symbol                  -> 55
 *   | TargetCompID            -> 56
 *   | Text                    -> 58
 *   | TimeInForce             -> 59
 *   | RawData                 -> 96
 *   | EncryptMethod           -> 98
 *   | HeartBtInt              -> 108
 *   | TestReqID               -> 112
 *   | ResetSeqNumFlag         -> 141
 *   | NoRelatedSym            -> 146
 *   | MDReqID                 -> 262
 *   | SubscriptionRequestType -> 263
 *   | MarketDepth             -> 264
 *   | MDUpdateType            -> 265
 *   | NoMDEntryTypes          -> 267
 *   | MDEntryType             -> 269
 *   | RefTagID                -> 371
 *   | RefMsgType              -> 372
 *   | Username                -> 553
 *   | Password                -> 554
 *   | TradeRequestID          -> 568
 *   | DefaultApplVerID        -> 1137
 *   | _ -> invalid_arg "tag_of_typ" *)
