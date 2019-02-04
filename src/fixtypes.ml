open Rresult
open Astring
open Sexplib.Std

module type IOMIN = sig
  type t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val parse : string -> t option
  val print : t -> string
end

module type IO = sig
  include IOMIN

  val parse_exn : string -> t
  val pp : Format.formatter -> t -> unit
  val pp_sexp : Format.formatter -> t -> unit
end

module Make (T : IOMIN) = struct
  open T
  let parse_exn s =
    match parse s with
    | None -> invalid_arg "parse"
    | Some v -> v

  let pp ppf v = Format.fprintf ppf "%s" (print v)
  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let date_of_string s =
    match String.(Sub.to_int (sub_with_range s ~first:0 ~len:4)),
          String.(Sub.to_int (sub_with_range s ~first:4 ~len:2)),
          String.(Sub.to_int (sub_with_range s ~first:6 ~len:2))
    with
    | Some y, Some m, Some d -> Some (y, m, d)
    | _ -> None

  let string_of_date (y, m, d) =
    Printf.sprintf "%04d%02d%02d" y m d

  let date_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match date_of_string sexp_str with
    | Some v -> v
    | _ -> invalid_arg "Ptime.date_of_sexp"

  let sexp_of_date d =
    sexp_of_string (string_of_date d)

  let tzspec =
    let open Tyre in
    let col = char ':' in
    let num2 = conv int_of_string string_of_int (pcre "\\d{2}") in
    let tz_offset_of_int i =
      let r = (i mod 3600) / 60 in
      (i / 3600, if r <> 0 then Some r else None) in
    conv
      begin function
        | `Left () -> 0
        | `Right ((`Left (), hh), None) -> hh * 3600
        | `Right ((`Left (), hh), Some mm) -> hh * 3600 + mm * 60
        | `Right ((`Right (), hh), None) -> ~- hh * 3600
        | `Right ((`Right (), hh), Some mm) -> ~- hh * 3600 + mm * 60
      end
      begin function
        | 0 -> `Left ()
        | off when off > 0 ->
          let hh, mm = tz_offset_of_int off in
          `Right (((`Left (), hh), mm))
        | off ->
          let hh, mm = tz_offset_of_int (~- off) in
          `Right (((`Right (), hh), mm))
      end
      (char 'Z' <|>
       ((char '+' <|> char '-') <&> num2 <&> opt (col *> num2)))

  let time =
    let open Tyre in
    let col = char ':' in
    let num2 = conv int_of_string string_of_int (pcre "\\d{2}") in
    conv
      begin function
        | (((hh, mm), None), None) -> (hh, mm, 0), 0
        | (((hh, mm), Some ss), None) -> (hh, mm, ss), 0
        | (((hh, mm), None), Some off) -> (hh, mm, 0), off
        | (((hh, mm), Some ss), Some off) -> (hh, mm, ss), off
      end
      begin function
        | ((hh, mm, 0), 0) -> (((hh, mm), None), None)
        | ((hh, mm, ss), 0) -> (((hh, mm), Some ss), None)
        | ((hh, mm, 0), off) -> (((hh, mm), None), Some off)
        | ((hh, mm, ss), off) -> (((hh, mm), Some ss), Some off)
      end
      (num2 <* col <&> num2 <&> opt (col *> num2) <&> opt tzspec)

  let time_re = Tyre.compile time

  let time_of_string s = Tyre.exec time_re s
  let time_of_string_opt s = R.to_option (time_of_string s)

  let time_of_sexp s =
    match time_of_string (string_of_sexp s) with
    | Ok time -> time
    | Error _  -> invalid_arg "time_of_sexp"

  let string_of_time t = Tyre.eval time t

  let sexp_of_time t =
    sexp_of_string (string_of_time t)
end

module Date = struct
  module T = struct
    type t = Ptime.date [@@deriving sexp]

    let parse = Ptime.date_of_string
    let print = Ptime.string_of_date
  end
  include T
  include Make(T)
end

module TZTimeOnly = struct
  module T = struct
    type t = Ptime.time [@@deriving sexp]

    let parse = Ptime.time_of_string_opt
    let print = Ptime.string_of_time
  end
  include T
  include Make(T)
end

module UTCTimestamp = struct
  let parse ?(ms=0) y m d h mm s =
    match Ptime.of_date_time ((y, m, d), ((h, mm, s), 0)),
          Ptime.Span.of_d_ps (0, Int64.(mul 1_000_000_000L (of_int ms)))
    with
    | Some ts, Some frac -> begin
        match Ptime.(add_span ts frac) with
        | None -> None
        | Some ts -> Some ts
      end
    | _ -> None

  let pp ppf t =
    let ((y, m, d), ((hh, mm, ss), _)) = Ptime.to_date_time t in
    let _, ps = Ptime.(Span.to_d_ps (frac_s t)) in
    match ps with
    | 0L -> Format.fprintf ppf "%d%02d%02d-%02d:%02d:%02d" y m d hh mm ss
    | _ ->
      Format.fprintf ppf "%d%02d%02d-%02d:%02d:%02d.%03Ld" y m d hh mm ss
        Int64.(div ps 1_000_000_000L)

  let re =
    let open Tyre in
    let dash = char '-' in
    let colon = char ':' in
    let dot = char '.' in
    let ms = dot *> int in
    let ci = conv int_of_string string_of_int in
    compile @@ conv
      (fun ((((((y, m), d), h), mm), s), ms) -> parse y m d h mm s ?ms)
      (function
        | None -> ((((((0, 0), 0), 0), 0), 0), None)
        | Some t ->
          match Ptime.to_date_time t
          with ((y, m, d), ((h, mm, s), _)) ->
            ((((((y, m), d), h), mm), s), None))
      (ci (pcre "\\d{4}") <&> ci (pcre "\\d{2}") <&> ci (pcre "\\d{2}") <*
       dash <&> int <* colon <&> int <* colon <&> int <&> (opt ms))

  let parse str =
    Tyre.exec re str

  let parse_opt str =
    match parse str with
    | Ok v -> v
    | _ -> None

  let parse_exn str =
    let open R in
    failwith_error_msg @@
    reword_error
      (fun e -> msg (Format.asprintf "%a" Tyre.pp_error e))
      (parse str) |> function
    | None -> failwith "invalid timestamp"
    | Some ts -> ts
end

module HandlInst = struct
  module T = struct
    type t =
      | Private
      | Public
      | Manual
    [@@deriving sexp]

    let parse = function
      | "1" -> Some Private
      | "2" -> Some Public
      | "3" -> Some Manual
      | _ -> None

    let print = function
      | Private -> "1"
      | Public -> "2"
      | Manual -> "3"
  end
  include T
  include Make(T)
end

module OrdStatus = struct
  module T = struct
    type t =
      | New
    [@@deriving sexp]

    let parse = function
      | "0" -> Some New
      | _ -> None

    let print = function
      | New -> "0"
  end
  include T
  include Make(T)
end

module OrdType = struct
  module T = struct
    type t =
      | Market
    [@@deriving sexp]

    let parse = function
      | "1" -> Some Market
      | _ -> None

    let print = function
      | Market -> "1"
  end
  include T
  include Make(T)
end


module PutOrCall = struct
  module T = struct
    type t =
      | Put
      | Call
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Put
      | "1" -> Some Call
      | _ -> None

    let print = function
      | Put -> "0"
      | Call -> "1"
  end
  include T
  include Make(T)
end

module EncryptMethod = struct
  module T = struct
    type t =
      | Other
      | PKCS
      | DES
      | PKCS_DES
      | PGP_DES
      | PGP_DES_MD5
      | PEM_DES_MD5
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Other
      | "1" -> Some PKCS
      | "2" -> Some DES
      | "3" -> Some PKCS_DES
      | "4" -> Some PGP_DES
      | "5" -> Some PGP_DES_MD5
      | "6" -> Some PEM_DES_MD5
      | _ -> None

    let print = function
      | Other -> "0"
      | PKCS -> "1"
      | DES -> "2"
      | PKCS_DES -> "3"
      | PGP_DES -> "4"
      | PGP_DES_MD5 -> "5"
      | PEM_DES_MD5 -> "6"
  end
  include T
  include Make(T)
end

module SubscriptionRequestType = struct
  module T = struct
    type t =
      | Snapshot
      | Subscribe
      | Unsubscribe
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Snapshot
      | "1" -> Some Subscribe
      | "2" -> Some Unsubscribe
      | _ -> None

    let print = function
      | Snapshot -> "0"
      | Subscribe -> "1"
      | Unsubscribe -> "2"
  end
  include T
  include Make(T)
end

module MdUpdateType = struct
  module T = struct
    type t =
      | Full
      | Incremental
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Full
      | "1" -> Some Incremental
      | _ -> None

    let print = function
      | Full -> "0"
      | Incremental -> "1"
  end
  include T
  include Make(T)
end

module MdEntryType = struct
  module T = struct
    type t =
      | Bid
      | Offer
      | Trade
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Bid
      | "1" -> Some Offer
      | "2" -> Some Trade
      | _ -> None

    let print = function
      | Bid -> "0"
      | Offer -> "1"
      | Trade -> "2"
  end
  include T
  include Make(T)
end

module Side = struct
  module T = struct
    type t =
      | Buy
      | Sell
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Buy
      | "1" -> Some Sell
      | _ -> None

    let print = function
      | Buy -> "0"
      | Sell -> "1"
  end
  include T
  include Make(T)
end

module TimeInForce = struct
  module T = struct
    type t =
      | Session
      | Good_till_cancel
      | At_the_opening
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Session
      | "1" -> Some Good_till_cancel
      | "2" -> Some At_the_opening
      | _ -> None

    let print = function
      | Session -> "0"
      | Good_till_cancel -> "1"
      | At_the_opening -> "2"
  end
  include T
  include Make(T)
end

module YesOrNo = struct
  module T = struct
    type t = bool [@@deriving sexp]

    let parse = function
      | "Y" -> Some true
      | "N" -> Some false
      | _ -> None

    let print = function
      | true -> "Y"
      | false -> "N"
  end
  include T
  include Make(T)
end

module SecurityListRequestType = struct
  module T = struct
    type t =
      | Symbol
      | SecurityType
      | Product
      | TradingSessionID
      | AllSecurities
      | MarketID
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Symbol
      | "1" -> Some SecurityType
      | "2" -> Some Product
      | "3" -> Some TradingSessionID
      | "4" -> Some AllSecurities
      | "5" -> Some MarketID
      | _ -> None

    let print = function
      | Symbol -> "0"
      | SecurityType -> "1"
      | Product -> "2"
      | TradingSessionID -> "3"
      | AllSecurities -> "4"
      | MarketID -> "5"
  end
  include T
  include Make(T)
end

module SecurityRequestResult = struct
  module T = struct
    type t =
      | Valid
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Valid
      | _ -> None

    let print = function
      | Valid -> "0"

  end
  include T
  include Make(T)
end

module SecurityType = struct
  module T = struct
    type t =
      | Future
      | Option
      | Index
    [@@deriving sexp]

    let parse = function
      | "FUT" -> Some Future
      | "OPT" -> Some Option
      | "INDEX" -> Some Index
      | _ -> None

    let print = function
      | Future -> "FUT"
      | Option -> "OPT"
      | Index -> "INDEX"
  end
  include T
  include Make(T)
end

module Version = struct
  module T = struct
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
  end
  include T
  include Make(T)
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
    | NewOrderList
    | OrderCancelRequest
    | OrderCancelReplaceRequest
    | OrderStatusRequest
    | MarketDataRequest
    | MarketDataSnapshotFullRefresh
    | MarketDataIncrementalRefresh
    | MarketDataRequestReject
    | SecurityListRequest
    | SecurityList
    | DerivativeSecurityListRequest
    | OrderMassStatusRequest
    | RequestForPositions
    | PositionReport
    | UserRequest
    | UserResponse
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let parse_exn = function
    | "0" -> Heartbeat
    | "1" -> TestRequest
    | "2" -> ResendRequest
    | "3" -> Reject
    | "4" -> SequenceReset
    | "5" -> Logout
    | "A" -> Logon
    | "D" -> NewOrderSingle
    | "E" -> NewOrderList
    | "F" -> OrderCancelRequest
    | "G" -> OrderCancelReplaceRequest
    | "H" -> OrderStatusRequest
    | "V" -> MarketDataRequest
    | "W" -> MarketDataSnapshotFullRefresh
    | "X" -> MarketDataIncrementalRefresh
    | "Y" -> MarketDataRequestReject
    | "x" -> SecurityListRequest
    | "y" -> SecurityList
    | "z" -> DerivativeSecurityListRequest
    | "AF" -> OrderMassStatusRequest
    | "AN" -> RequestForPositions
    | "AP" -> PositionReport
    | "BE" -> UserRequest
    | "BF" -> UserResponse
    | s -> invalid_arg ("MsgType: unknown msg type " ^ s)

  let parse s =
    try Some (parse_exn s) with _ -> None

  let print = function
    | Heartbeat                     -> "0"
    | TestRequest                   -> "1"
    | ResendRequest                 -> "2"
    | Reject                        -> "3"
    | SequenceReset                 -> "4"
    | Logout                        -> "5"
    | Logon                         -> "A"
    | NewOrderSingle                -> "D"
    | NewOrderList                  -> "E"
    | OrderCancelRequest            -> "F"
    | OrderCancelReplaceRequest     -> "G"
    | OrderStatusRequest            -> "H"
    | MarketDataRequest             -> "V"
    | MarketDataSnapshotFullRefresh -> "W"
    | MarketDataIncrementalRefresh  -> "X"
    | MarketDataRequestReject       -> "Y"
    | SecurityListRequest           -> "x"
    | SecurityList                  -> "y"
    | DerivativeSecurityListRequest -> "z"
    | OrderMassStatusRequest        -> "AF"
    | RequestForPositions           -> "AN"
    | PositionReport                -> "AP"
    | UserRequest                   -> "BE"
    | UserResponse                  -> "BF"

  let pp ppf t =
    Format.fprintf ppf "%s" (print t)
end

module SessionRejectReason = struct
  type t =
    | InvalidTag
    | MissingTag
    | TagNotDefinedForThisMessage
    | UndefinedTag
    | TagWithoutValue
    | TagValueIncorrect
    | IncorrectData
    | DecryptionProblem
    | SignatureProblem
    | CompIDProblem
    | SendingTimeAccuracy
    | InvalidMsgType
    | XMLValidationError
    | InvalidVersion
    | Other
  [@@deriving sexp]

  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let of_int_exn = function
    | 0 -> InvalidTag
    | 1 -> MissingTag
    | 2 -> TagNotDefinedForThisMessage
    | 3 -> UndefinedTag
    | _ -> invalid_arg "SessionRejectReason.of_int"

  let of_int i =
    try Some (of_int_exn i) with _ -> None

  let to_int = function
    | InvalidTag -> 0
    | MissingTag -> 1
    | TagNotDefinedForThisMessage -> 2
    | UndefinedTag -> 3
    | TagWithoutValue -> 4
    | TagValueIncorrect -> 5
    | IncorrectData -> 6
    | DecryptionProblem -> 7
    | SignatureProblem -> 8
    | CompIDProblem -> 9
    | SendingTimeAccuracy -> 10
    | InvalidMsgType -> 11
    | XMLValidationError -> 12
    | InvalidVersion -> 18
    | Other -> 99

  let parse s =
    match int_of_string_opt s with
    | None -> None
    | Some i -> of_int i

  let parse_exn s =
    match parse s with
    | Some v -> v
    | None -> invalid_arg "SessionRejectReason.parse_exn"

  let print t = string_of_int (to_int t)

  let pp ppf t = Format.pp_print_int ppf (to_int t)
end
