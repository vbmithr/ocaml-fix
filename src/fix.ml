open Rresult
open Astring
open Sexplib.Std

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Timestamp.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)
end

module UTCTimestamp = struct
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
        | None -> None
        | Some ts -> Some ts
      end
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "UTCTimestamp.parse"
    | Some v -> v

  let pp ppf t =
    let ((y, m, d), ((hh, mm, ss), _)) = Ptime.to_date_time t in
    Format.fprintf ppf "%d%d%d-%d:%d:%d" y m d hh mm ss
end

module HandlInst = struct
  type t =
    | Private
    | Public
    | Manual
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "1" -> Some Private
    | "2" -> Some Public
    | "3" -> Some Manual
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "HandlInst.parse"
    | Some v -> v

  let print = function
    | Private -> "1"
    | Public -> "2"
    | Manual -> "3"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module OrdStatus = struct
  type t =
    | New
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "0" -> Some New
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "OrdStatus.parse"
    | Some v -> v

  let print = function
    | New -> "0"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module OrdType = struct
  type t =
    | Market
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "1" -> Some Market
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "OrdType.parse"
    | Some v -> v

  let print = function
    | Market -> "1"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
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
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "0" -> Some Other
    | "1" -> Some PKCS
    | "2" -> Some DES
    | "3" -> Some PKCS_DES
    | "4" -> Some PGP_DES
    | "5" -> Some PGP_DES_MD5
    | "6" -> Some PEM_DES_MD5
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "EncryptMethod.parse"
    | Some v -> v

  let print = function
    | Other -> "0"
    | PKCS -> "1"
    | DES -> "2"
    | PKCS_DES -> "3"
    | PGP_DES -> "4"
    | PGP_DES_MD5 -> "5"
    | PEM_DES_MD5 -> "6"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module SubscriptionRequestType = struct
  type t =
    | Snapshot
    | Subscribe
    | Unsubscribe
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "0" -> Some Snapshot
    | "1" -> Some Subscribe
    | "2" -> Some Unsubscribe
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "SubscriptionRequestType.parse"
    | Some v -> v

  let print = function
    | Snapshot -> "0"
    | Subscribe -> "1"
    | Unsubscribe -> "2"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module MdUpdateType = struct
  type t =
    | Full
    | Incremental
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "0" -> Some Full
    | "1" -> Some Incremental
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "MdUpdateType.parse"
    | Some v -> v

  let print = function
    | Full -> "0"
    | Incremental -> "1"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module MdEntryType = struct
  type t =
    | Bid
    | Offer
    | Trade
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "0" -> Some Bid
    | "1" -> Some Offer
    | "2" -> Some Trade
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "MdEntryType.parse"
    | Some v -> v

  let print = function
    | Bid -> "0"
    | Offer -> "1"
    | Trade -> "2"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module Side = struct
  type t =
    | Buy
    | Sell
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "0" -> Some Buy
    | "1" -> Some Sell
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "Side.parse"
    | Some v -> v

  let print = function
    | Buy -> "0"
    | Sell -> "1"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module TimeInForce = struct
  type t =
    | Session
    | Good_till_cancel
    | At_the_opening
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse = function
    | "0" -> Some Session
    | "1" -> Some Good_till_cancel
    | "2" -> Some At_the_opening
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "TimeInForce.parse"
    | Some v -> v

  let print = function
    | Session -> "0"
    | Good_till_cancel -> "1"
    | At_the_opening -> "2"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module YesOrNo = struct
  let parse = function
    | "Y" -> Some true
    | "N" -> Some false
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "YesOrNo.parse"
    | Some v -> v

  let print = function
    | true -> "Y"
    | false -> "N"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module Version = struct
  type t =
    | FIX of int * int
    | FIXT of int * int
  [@@deriving sexp]

  let v40 = FIX (4, 0)
  let v41 = FIX (4, 1)
  let v42 = FIX (4, 2)
  let v43 = FIX (4, 3)
  let v44 = FIX (4, 4)
  let v5  = FIXT (1, 1)

  let pp ppf = function
    | FIX  (major, minor) -> Format.fprintf ppf "FIX.%d.%d" major minor
    | FIXT (major, minor) -> Format.fprintf ppf "FIXT.%d.%d" major minor

  let print t = Format.asprintf "%a" pp t

  let parse s =
    match String.cuts ~sep:"." s with
    | [ "FIX" ; major ; minor ] ->
      Some (FIX (int_of_string major, int_of_string minor))
    | [ "FIXT" ; major ; minor ] ->
      Some (FIXT (int_of_string major, int_of_string minor))
    | _ -> None

  let parse_exn s =
    match parse s with
    | None -> invalid_arg "Version.parse"
    | Some v -> v
end

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
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse_exn s =
    failwith "not implemented"

  let print = function
    | Heartbeat         -> "0"
    | TestRequest       -> "1"
    | ResendRequest     -> "2"
    | Reject            -> "3"
    | SequenceReset     -> "4"
    | Logout            -> "5"
    | Logon             -> "A"
    | NewOrderSingle    -> "D"
    | MarketDataRequest -> "V"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module Field = struct
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

  type (_,_) eq = Eq : ('a,'a) eq

  let eq_typ :
    type a b. a typ -> b typ -> (a,b) eq option = fun a b ->
    match a, b with
    | Account, Account -> Some Eq
    | BeginSeqNo, BeginSeqNo -> Some Eq
    | BeginString, BeginString -> Some Eq
    | _ -> None

  let tag_of_typ (type a) (typ : a typ) =
    match typ with
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
    | NewSeqNo                -> 36
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
    | DefaultApplVerID        -> 1137

  let pp_typ (type a) ppf (t : a typ) =
    Format.fprintf ppf "<not implemented>"

  type t = F : 'a typ * (Format.formatter -> 'a -> unit) * 'a -> t

  let create typ pp t = F (typ, pp, t)
  let msgtype mt = F (MsgType, MsgType.pp, mt)
  let beginstring version = F (BeginString, Version.pp, version)
  let sendercompid id = F (SenderCompID, Format.pp_print_string, id)
  let targetcompid id = F (TargetCompID, Format.pp_print_string, id)
  let msgseqnum i = F (MsgSeqNum, Format.pp_print_int, i)
  let rawdata s = F (RawData, Format.pp_print_string, s)
  let password s = (Password, Format.pp_print_string, s)

  let find :
    type a. a typ -> t -> a option =
    fun typ (F (typ', _, v)) ->
    match eq_typ typ typ' with
    | None -> None
    | Some Eq -> Some v

  exception Found_field of t

  let find_list typ ts =
    try
      List.iter begin fun t ->
        match find typ t with
        | None -> ()
        | Some _ -> raise (Found_field t)
      end ts ;
      None
    with Found_field t -> find typ t

  let pp ppf (F (typ, pp, v)) =
    Format.fprintf ppf "@[<v 1>%a: %a@]" pp_typ typ pp v

  let parse str =
    match String.cut ~sep:"=" str with
    | None -> R.error_msg "Missing '='"
    | Some (tag, value) ->
      match int_of_string_opt tag with
      | None -> R.error_msg "Tag is not an int value"
      | Some tag ->
        let open Format in
        try R.ok @@
          match tag with
          | 1   -> create Account pp_print_string value
          | 7   -> create BeginSeqNo pp_print_int (int_of_string value)
          | 8   -> create BeginString Version.pp (Version.parse_exn value)
          | 9   -> create BodyLength pp_print_int (int_of_string value)
          | 10  -> create CheckSum pp_print_string value
          | 11  -> create ClOrdID pp_print_string value
          | 16  -> create EndSeqNo pp_print_int (int_of_string value)
          | 21  -> create HandlInst HandlInst.pp (HandlInst.parse_exn value)
          | 34  -> create MsgSeqNum pp_print_int (int_of_string value)
          | 35  -> create MsgType MsgType.pp (MsgType.parse_exn value)
          | 46  -> create NewSeqNo pp_print_int (int_of_string value)
          | 37  -> create OrderID pp_print_string value
          | 38  -> create OrderQty pp_print_float (float_of_string value)
          | 39  -> create OrdStatus OrdStatus.pp (OrdStatus.parse_exn value)
          | 40  -> create OrdType OrdType.pp (OrdType.parse_exn value)
          | 44  -> create Price pp_print_float (float_of_string value)
          | 45  -> create RefSeqNum pp_print_int (int_of_string value)
          | 49  -> create SenderCompID pp_print_string value
          | 52  -> create SendingTime UTCTimestamp.pp (UTCTimestamp.parse_exn value)
          | 54  -> create Side Side.pp (Side.parse_exn value)
          | 55  -> create Symbol pp_print_string value
          | 56  -> create TargetCompID pp_print_string value
          | 58  -> create Text pp_print_string value
          | 59  -> create TimeInForce TimeInForce.pp (TimeInForce.parse_exn value)
          | 96  -> create RawData pp_print_string value
          | 98  -> create EncryptMethod EncryptMethod.pp (EncryptMethod.parse_exn value)
          | 108 -> create HeartBtInt pp_print_int (int_of_string value)
          | 112 -> create TestReqID pp_print_string value
          | 141 -> create ResetSeqNumFlag YesOrNo.pp (YesOrNo.parse_exn value)
          | 146 -> create NoRelatedSym pp_print_int (int_of_string value)
          | 262 -> create MDReqID pp_print_string value
          | 263 -> create SubscriptionRequestType SubscriptionRequestType.pp (SubscriptionRequestType.parse_exn value)
          | 264 -> create MarketDepth pp_print_int (int_of_string value)
          | 265 -> create MDUpdateType MdUpdateType.pp (MdUpdateType.parse_exn value)
          | 267 -> create NoMDEntryTypes pp_print_int (int_of_string value)
          | 269 -> create MDEntryType MdEntryType.pp (MdEntryType.parse_exn value)
          | 371 -> create RefTagID pp_print_int (int_of_string value)
          | 372 -> create RefMsgType MsgType.pp (MsgType.parse_exn value)
          | 553 -> create Username pp_print_string value
          | 554 -> create Password pp_print_string value
          | 568 -> create TradeRequestID pp_print_string value
          | 1137 -> create DefaultApplVerID pp_print_string value
          | i   -> invalid_arg ("Unknown tag " ^ string_of_int i)
        with
        | Invalid_argument msg -> R.error_msg msg

  let print (F (typ, pp, v)) =
    Format.asprintf "%d=%a" (tag_of_typ typ) pp v

  let add_to_buffer buf (F (typ, pp, v)) =
    let open Buffer in
    add_string buf (string_of_int (tag_of_typ typ)) ;
    add_char buf '=' ;
    add_string buf (Format.asprintf "%a" pp v)

  let sexp_of_t t = sexp_of_string (print t)
  let t_of_sexp sexp =
    R.failwith_error_msg (parse (string_of_sexp sexp))
end

type t = {
  typ : MsgType.t ;
  fields : Field.t list ;
} [@@deriving sexp]

let create ~typ ~fields = { typ ; fields }

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

let to_bytes ?(buf = Buffer.create 128) ~version { typ ; fields } =
  let add_field buf tag value =
    Buffer.add_string buf tag;
    Buffer.add_char buf '=';
    Buffer.add_string buf value;
    Buffer.add_char buf '\x01'
  in
  let fields = Field.msgtype typ :: fields in
  ListLabels.iter fields ~f:begin fun f ->
    Field.add_to_buffer buf f ;
    Buffer.add_char buf '\x01'
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
    ListLabels.fold_left fields ~init:(0, 0) ~f:begin fun (global, _local) sub ->
      let count = String.Sub.fold_left (fun a c -> a + Char.to_int c) 1 sub in
      global + count, count
    end in
  (global - local) mod 256

let read ?pos ?len buf =
  let buf = String.sub_with_range ?first:pos ?len buf in
  let fields = String.Sub.cuts ~sep:(String.Sub.of_char '\x01') buf in
  let computed_chksum = compute_chksum fields in
  begin try
      ListLabels.fold_left fields ~init:[] ~f:begin fun a f ->
        match Field.parse (String.Sub.to_string f) with
        | Error _ as e -> R.failwith_error_msg e
        | Ok v -> v :: a
      end |> R.ok
    with Failure msg -> R.error_msg msg
  end |> function
  | Error _ as e -> e
  | Ok [] -> R.error_msg "empty message"
  | Ok (chksum :: fields) ->
    match Field.find CheckSum chksum,
          Field.find_list MsgType fields with
    | None, _ -> R.error_msg "missing checksum"
    | Some _, None -> R.error_msg "missing MsgType"
    | Some chksum, Some typ ->
      if computed_chksum <> (int_of_string chksum) then
        R.error_msg "bad checksum"
      else R.ok (create ~typ ~fields)
