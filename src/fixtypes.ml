(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Sexplib.Std

module Yojsonable = struct
  module type S = sig
    type t
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> (t, string) result
  end
end

module type IOMIN = sig
  type t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val parse : string -> (t, R.msg) result
  val to_string : t -> string
end

module type IO = sig
  include IOMIN

  val parse_exn : string -> t
  val pp_fix : t Fmt.t
  val pp_sexp : t Fmt.t
  val encoding : t Json_encoding.encoding
end

module Make (T : IOMIN) = struct
  open T
  let parse_exn s = R.failwith_error_msg (parse s)
  let pp_fix ppf v = Format.fprintf ppf "%s" (to_string v)
  let pp_sexp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
  let encoding =
    Json_encoding.(conv to_string parse_exn string)
end

let of_yojson_string ~f = function
  | `String s -> f s
  | #Yojson.Safe.t -> Error "input must be a json string"

let reword_tyre_error t =
  R.reword_error (fun e -> R.msgf "%a" Tyre.pp_error e) t

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let of_yojson =
    of_yojson_string ~f:begin fun s ->
      match of_rfc3339 s with
      | Ok (v,_,_) -> Ok v
      | Error (`RFC3339 (_, e)) ->
        R.fail (Format.asprintf "%a" pp_rfc3339_error e)
    end

  let to_yojson t = `String (to_rfc3339 t)

  let date_of_string s =
    if String.length s <> 8 then
      R.error_msg "date_of_string: string must have length 8"
    else
      match String.(Sub.to_int (sub_with_range s ~first:0 ~len:4)),
            String.(Sub.to_int (sub_with_range s ~first:4 ~len:2)),
            String.(Sub.to_int (sub_with_range s ~first:6 ~len:2))
      with
      | Some y, Some m, Some d -> Ok (y, m, d)
      | _ -> R.error_msg "date_of_string: parse error"

  let string_of_date (y, m, d) =
    Printf.sprintf "%04d%02d%02d" y m d

  let date_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    R.failwith_error_msg (date_of_string sexp_str)

  let sexp_of_date d =
    sexp_of_string (string_of_date d)

  let date_of_yojson = of_yojson_string ~f:begin fun s ->
      R.reword_error (function `Msg s -> s) (date_of_string s)
    end

  let date_to_yojson d = `String (string_of_date d)

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
        | ((hh, mm), None), off -> (hh, mm, 0), off
        | ((hh, mm), Some ss), off -> (hh, mm, ss), off
      end
      begin function
        | (hh, mm, 0), off -> ((hh, mm), None), off
        | (hh, mm, ss), off -> ((hh, mm), Some ss), off
      end
      (num2 <* col <&> num2 <&> opt (col *> num2) <&> tzspec)

  let time_re = Tyre.compile time

  let time_of_string s = reword_tyre_error (Tyre.exec time_re s)

  let time_of_sexp s =
    R.failwith_error_msg (time_of_string (string_of_sexp s))

  let time_of_yojson = of_yojson_string ~f:begin fun s ->
      R.reword_error (function `Msg m -> m) (time_of_string s)
    end

  let pp_time ppf = function
    | (hh, mm, 0), 0 -> Format.fprintf ppf "%02d:%02dZ" hh mm
    | (hh, mm, ss), 0 -> Format.fprintf ppf "%02d:%02d:%02dZ" hh mm ss
    | (hh, mm, 0), off when off > 0 -> begin
        match off / 3600, off mod 3600 with
        | hhhh, 0 ->
          Format.fprintf ppf "%02d:%02d+%02d" hh mm hhhh
        | hhhh, _ ->
          let mmmm = hhhh / 60 in
          Format.fprintf ppf "%02d:%02d+%02d:%02d" hh mm hhhh mmmm
      end
    | (hh, mm, 0), off -> begin
        match off / 3600, off mod 3600 with
        | hhhh, 0 ->
          Format.fprintf ppf "%02d:%02d-%02d" hh mm hhhh
        | hhhh, _ ->
          let mmmm = hhhh / 60 in
          Format.fprintf ppf "%02d:%02d-%02d:%02d" hh mm hhhh mmmm
      end
    | (hh, mm, ss), off when off > 0 -> begin
        match off / 3600, off mod 3600 with
        | hhhh, 0 ->
          Format.fprintf ppf "%02d:%02d:%02d+%02d" hh mm ss hhhh
        | hhhh, _ ->
          let mmmm = hhhh / 60 in
          Format.fprintf ppf "%02d:%02d:%02d+%02d:%02d" hh mm ss hhhh mmmm
      end
    | (hh, mm, ss), off -> begin
        match off / 3600, off mod 3600 with
        | hhhh, 0 ->
          Format.fprintf ppf "%02d:%02d:%02d-%02d" hh mm ss hhhh
        | hhhh, _ ->
          let mmmm = hhhh / 60 in
          Format.fprintf ppf "%02d:%02d:%02d-%02d:%02d" hh mm ss hhhh mmmm
      end

  let string_of_time t =
    Format.asprintf "%a" pp_time t

  let sexp_of_time t =
    sexp_of_string (string_of_time t)

  let time_to_yojson t = `String (string_of_time t)
end

module Uuidm = struct
  include Uuidm

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_string sexp_str with
    | None -> invalid_arg "Uuidm.t_of_sexp"
    | Some u -> u

  let sexp_of_t t =
    sexp_of_string (to_string t)

  let of_yojson = function
    | `String s -> begin
      match of_string s with
      | None -> Error "not an uuid"
      | Some u -> Ok u
    end
    | #Yojson.Safe.t -> Error "not a json string"

  let to_yojson t = `String (to_string t)
end

module Date = struct
  module T = struct
    type t = Ptime.date [@@deriving sexp,yojson]

    let parse = Ptime.date_of_string
    let to_string = Ptime.string_of_date
  end
  include T
  include Make(T)
end

module TZTimeOnly = struct
  module T = struct
    type t = Ptime.time [@@deriving sexp,yojson]

    let parse = Ptime.time_of_string
    let to_string = Ptime.string_of_time
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
        | None -> invalid_arg "not an UTCTimestamp"
        | Some ts -> ts
      end
    | _ -> invalid_arg "not an UTCTimestamp"

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
      (fun t ->
         match Ptime.to_date_time t
         with ((y, m, d), ((h, mm, s), _)) ->
           ((((((y, m), d), h), mm), s), None))
      (ci (pcre "\\d{4}") <&> ci (pcre "\\d{2}") <&> ci (pcre "\\d{2}") <*
       dash <&> int <* colon <&> int <* colon <&> int <&> (opt ms))

  let parse str = reword_tyre_error (Tyre.exec re str)
  let parse_exn str = R.failwith_error_msg (parse str)
end

module UserRequestType = struct
  module T = struct
    type t =
      | Logon
      | Logoff
      | ChangePassword
      | RequestStatus
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok Logon
      | "2" -> Ok Logoff
      | "3" -> Ok ChangePassword
      | "4" -> Ok RequestStatus
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Logon          -> "1"
      | Logoff         -> "2"
      | ChangePassword -> "3"
      | RequestStatus  -> "4"
  end
  include T
  include Make(T)
end

module UserStatus = struct
  module T = struct
    type t =
      | LoggedIn
      | LoggedOff
      | NotRecognized
      | PasswordIncorrect
      | PasswordChanged
      | Other
      | ForcedLogout
      | SessionShutdownWarning
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok LoggedIn
      | "2" -> Ok LoggedOff
      | "3" -> Ok NotRecognized
      | "4" -> Ok PasswordIncorrect
      | "5" -> Ok PasswordChanged
      | "6" -> Ok Other
      | "7" -> Ok ForcedLogout
      | "8" -> Ok SessionShutdownWarning
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | LoggedIn               -> "1"
      | LoggedOff              -> "2"
      | NotRecognized          -> "3"
      | PasswordIncorrect      -> "4"
      | PasswordChanged        -> "5"
      | Other                  -> "6"
      | ForcedLogout           -> "7"
      | SessionShutdownWarning -> "8"
  end
  include T
  include Make(T)
end

module HandlInst = struct
  module T = struct
    type t =
      | Private
      | Public
      | Manual
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok Private
      | "2" -> Ok Public
      | "3" -> Ok Manual
      | _ -> R.error_msg "unknown code"

    let to_string = function
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
      | PartiallyFilled
      | Filled
      | DoneForDay
      | Canceled
      | Replaced
      | PendingCancel
      | Stopped
      | Rejected
      | Suspended
      | PendingNew
      | Calculated
      | Expired
      | AcceptedForBidding
      | PendingReplace
    [@@deriving sexp,yojson,bin_io,show]

    let parse = function
      | "0" -> Ok New
      | "1" -> Ok PartiallyFilled
      | "2" -> Ok Filled
      | "3" -> Ok DoneForDay
      | "4" -> Ok Canceled
      | "5" -> Ok Replaced
      | "6" -> Ok PendingCancel
      | "7" -> Ok Stopped
      | "8" -> Ok Rejected
      | "9" -> Ok Suspended
      | "A" -> Ok PendingNew
      | "B" -> Ok Calculated
      | "C" -> Ok Expired
      | "D" -> Ok AcceptedForBidding
      | "E" -> Ok PendingReplace
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | New                -> "0"
      | PartiallyFilled    -> "1"
      | Filled             -> "2"
      | DoneForDay         -> "3"
      | Canceled           -> "4"
      | Replaced           -> "5"
      | PendingCancel      -> "6"
      | Stopped            -> "7"
      | Rejected           -> "8"
      | Suspended          -> "9"
      | PendingNew         -> "A"
      | Calculated         -> "B"
      | Expired            -> "C"
      | AcceptedForBidding -> "D"
      | PendingReplace     -> "E"
  end
  include T
  include Make(T)
end

module PosReqType = struct
  module T = struct
    type t =
      | Positions
      | Trades
      | Exercises
      | Assignments
      | SettlementActivity
      | BackoutMessage
      | DeltaPositions
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Positions
      | "1" -> Ok Trades
      | "2" -> Ok Exercises
      | "3" -> Ok Assignments
      | "4" -> Ok SettlementActivity
      | "5" -> Ok BackoutMessage
      | "6" -> Ok DeltaPositions
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Positions          -> "0"
      | Trades             -> "1"
      | Exercises          -> "2"
      | Assignments        -> "3"
      | SettlementActivity -> "4"
      | BackoutMessage     -> "5"
      | DeltaPositions     -> "6"
  end
  include T
  include Make(T)
end

module MassStatusReqType = struct
  module T = struct
    type t =
      | Security
      | UnderlyingSecurity
      | Product
      | CFICode
      | SecurityType
      | TradingSession
      | AllOrders
      | PartyID
      | SecurityIssuer
      | UssuerOfUnderlyingSecurity
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok Security
      | "2" -> Ok UnderlyingSecurity
      | "3" -> Ok Product
      | "4" -> Ok CFICode
      | "5" -> Ok SecurityType
      | "6" -> Ok TradingSession
      | "7" -> Ok AllOrders
      | "8" -> Ok PartyID
      | "9" -> Ok SecurityIssuer
      | "10" -> Ok UssuerOfUnderlyingSecurity
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Security                   -> "1"
      | UnderlyingSecurity         -> "2"
      | Product                    -> "3"
      | CFICode                    -> "4"
      | SecurityType               -> "5"
      | TradingSession             -> "6"
      | AllOrders                  -> "7"
      | PartyID                    -> "8"
      | SecurityIssuer             -> "9"
      | UssuerOfUnderlyingSecurity -> "10"
  end
  include T
  include Make(T)
end

module PosReqResult = struct
  module T = struct
    type t =
      | ValidRequest
      | InvalidRequest
      | NoPositionsFound
      | NotAuthorized
      | Unsupported
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok ValidRequest
      | "1" -> Ok InvalidRequest
      | "2" -> Ok NoPositionsFound
      | "3" -> Ok NotAuthorized
      | "4" -> Ok Unsupported
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | ValidRequest     -> "0"
      | InvalidRequest   -> "1"
      | NoPositionsFound -> "2"
      | NotAuthorized    -> "3"
      | Unsupported      -> "4"
  end
  include T
  include Make(T)
end

module OrdType = struct
  module T = struct
    type t =
      | Market
      | Limit
      | Stop
      | StopLimit
      | MarketIfTouched
      | LimitIfTouched
    [@@deriving sexp,yojson,bin_io,show]

    let parse = function
      | "1" -> Ok Market
      | "2" -> Ok Limit
      | "3" -> Ok Stop
      | "4" -> Ok StopLimit
      | "J" -> Ok MarketIfTouched
      | "K" -> Ok LimitIfTouched
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Market        -> "1"
      | Limit         -> "2"
      | Stop          -> "3"
      | StopLimit     -> "4"
      | MarketIfTouched -> "J"
      | LimitIfTouched -> "K"
  end
  include T
  include Make(T)
end

module OrdRejReason = struct
  module T = struct
    type t =
      | Broker
      | UnknownSymbol
      | ExchangeClosed
      | OrderExceedsLimit
      | TooLateToEnter
      | UnknownOrder
      | DuplicateOrder
      | StaleOrder
    [@@deriving sexp,yojson,bin_io,show]

    let parse = function
      | "0" -> Ok Broker
      | "1" -> Ok UnknownSymbol
      | "2" -> Ok ExchangeClosed
      | "3" -> Ok OrderExceedsLimit
      | "4" -> Ok TooLateToEnter
      | "5" -> Ok UnknownOrder
      | "6" -> Ok DuplicateOrder
      | "8" -> Ok StaleOrder
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Broker            -> "0"
      | UnknownSymbol     -> "1"
      | ExchangeClosed    -> "2"
      | OrderExceedsLimit -> "3"
      | TooLateToEnter    -> "4"
      | UnknownOrder      -> "5"
      | DuplicateOrder    -> "6"
      | StaleOrder        -> "8"
  end
  include T
  include Make(T)
end

module PutOrCall = struct
  module T = struct
    type t =
      | Put
      | Call
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Put
      | "1" -> Ok Call
      | _ -> R.error_msg "unknown code"

    let to_string = function
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
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Other
      | "1" -> Ok PKCS
      | "2" -> Ok DES
      | "3" -> Ok PKCS_DES
      | "4" -> Ok PGP_DES
      | "5" -> Ok PGP_DES_MD5
      | "6" -> Ok PEM_DES_MD5
      | _ -> R.error_msg "unknown code"

    let to_string = function
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

module ExecTransType = struct
  module T = struct
    type t =
      | New
      | Cancel
      | Correct
      | Status
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok New
      | "1" -> Ok Cancel
      | "2" -> Ok Correct
      | "3" -> Ok Status
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | New     -> "0"
      | Cancel  -> "1"
      | Correct -> "2"
      | Status  -> "3"
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
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Snapshot
      | "1" -> Ok Subscribe
      | "2" -> Ok Unsubscribe
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Snapshot -> "0"
      | Subscribe -> "1"
      | Unsubscribe -> "2"
  end
  include T
  include Make(T)
end

module MDUpdateType = struct
  module T = struct
    type t =
      | Full
      | Incremental
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Full
      | "1" -> Ok Incremental
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Full -> "0"
      | Incremental -> "1"
  end
  include T
  include Make(T)
end

module MDUpdateAction = struct
  module T = struct
    type t =
      | New
      | Change
      | Delete
      | DeleteThru
      | DeleteFrom
      | Overlay
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok New
      | "1" -> Ok Change
      | "2" -> Ok Delete
      | "3" -> Ok DeleteThru
      | "4" -> Ok DeleteFrom
      | "5" -> Ok Overlay
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | New -> "0"
      | Change -> "1"
      | Delete -> "2"
      | DeleteThru -> "3"
      | DeleteFrom -> "4"
      | Overlay -> "5"
  end
  include T
  include Make(T)
end

module MDEntryType = struct
  module T = struct
    type t =
      | Bid
      | Offer
      | Trade
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Bid
      | "1" -> Ok Offer
      | "2" -> Ok Trade
      | _ -> R.error_msg "unknown code"

    let to_string = function
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
    [@@deriving sexp,yojson,bin_io,show]

    let parse = function
      | "1" -> Ok Buy
      | "2" -> Ok Sell
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Buy -> "1"
      | Sell -> "2"
  end
  include T
  include Make(T)
end

module TimeInForce = struct
  module T = struct
    type t =
      | Session
      | GoodTillCancel
      | AtTheOpening
      | ImmediateOrCancel
      | FillOrKill
      | GoodTillCrossing
      | GoodTillDate
      | AtTheClose
      | GoodThroughCrossing
      | AtCrossing
      | PostOnly (* Coinbase special *)
    [@@deriving sexp,yojson,bin_io,show]

    let parse = function
      | "0" -> Ok Session
      | "1" -> Ok GoodTillCancel
      | "2" -> Ok AtTheOpening
      | "3" -> Ok ImmediateOrCancel
      | "4" -> Ok FillOrKill
      | "5" -> Ok GoodTillCrossing
      | "6" -> Ok GoodTillDate
      | "7" -> Ok AtTheClose
      | "8" -> Ok GoodThroughCrossing
      | "9" -> Ok AtCrossing
      | "P" -> Ok PostOnly
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Session -> "0"
      | GoodTillCancel -> "1"
      | AtTheOpening -> "2"
      | ImmediateOrCancel -> "3"
      | FillOrKill -> "4"
      | GoodTillCrossing -> "5"
      | GoodTillDate -> "6"
      | AtTheClose -> "7"
      | GoodThroughCrossing -> "8"
      | AtCrossing -> "9"
      | PostOnly -> "P"
  end
  include T
  include Make(T)
end

module YesOrNo = struct
  module T = struct
    type t = bool [@@deriving sexp,yojson,show]

    let parse = function
      | "Y" -> Ok true
      | "N" -> Ok false
      | _ -> R.error_msg "unknown code"

    let to_string = function
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
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Symbol
      | "1" -> Ok SecurityType
      | "2" -> Ok Product
      | "3" -> Ok TradingSessionID
      | "4" -> Ok AllSecurities
      | "5" -> Ok MarketID
      | _ -> R.error_msg "unknown code"

    let to_string = function
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
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Valid
      | _ -> R.error_msg "unknown code"

    let to_string = function
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
    [@@deriving sexp,yojson,show]

    let parse = function
      | "FUT" -> Ok Future
      | "OPT" -> Ok Option
      | "INDEX" -> Ok Index
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Future -> "FUT"
      | Option -> "OPT"
      | Index -> "INDEX"
  end
  include T
  include Make(T)
end

module QtyType = struct
  module T = struct
    type t =
      | Units
      | Contracts
      | UnitsPerTime
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok Units
      | "1" -> Ok Contracts
      | "2" -> Ok UnitsPerTime
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Units -> "0"
      | Contracts -> "1"
      | UnitsPerTime -> "2"
  end
  include T
  include Make(T)
end

module Version = struct
  module T = struct
    type t =
      | FIX of int * int
      | FIXT of int * int
    [@@deriving sexp,yojson,show]

    let v40 = FIX (4, 0)
    let v41 = FIX (4, 1)
    let v42 = FIX (4, 2)
    let v43 = FIX (4, 3)
    let v44 = FIX (4, 4)
    let v5  = FIXT (1, 1)

    let pp ppf = function
      | FIX  (major, minor) -> Format.fprintf ppf "FIX.%d.%d" major minor
      | FIXT (major, minor) -> Format.fprintf ppf "FIXT.%d.%d" major minor

    let to_string t = Format.asprintf "%a" pp t

    let to_yojson t = `String (Format.asprintf "%a" pp t)

    let parse s =
      match String.cuts ~sep:"." s with
      | [ "FIX" ; major ; minor ] ->
        Ok (FIX (int_of_string major, int_of_string minor))
      | [ "FIXT" ; major ; minor ] ->
        Ok (FIXT (int_of_string major, int_of_string minor))
      | _ -> R.error_msg "unknown code"

    let of_yojson = of_yojson_string ~f:begin fun s ->
        R.reword_error (function `Msg m -> m) (parse s)
      end
  end
  include T
  include Make(T)
end

module MsgType = struct
  module T = struct
    type t =
      | Heartbeat
      | TestRequest
      | ResendRequest
      | Reject
      | SequenceReset
      | Logout
      | IOI
      | Advertisement
      | ExecutionReport
      | OrderCancelReject
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

      | NewOrderBatch
      | NewOrderBatchReject

      | OrderCancelBatchRequest
      | OrderCancelBatchReject
    [@@deriving sexp,yojson,show]

    let parse_exn = function
      | "0" -> Heartbeat
      | "1" -> TestRequest
      | "2" -> ResendRequest
      | "3" -> Reject
      | "4" -> SequenceReset
      | "5" -> Logout
      | "6" -> IOI
      | "7" -> Advertisement
      | "8" -> ExecutionReport
      | "9" -> OrderCancelReject
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
      | "U6" -> NewOrderBatch
      | "U7" -> NewOrderBatchReject
      | "U4" -> OrderCancelBatchRequest
      | "U5" -> OrderCancelBatchReject
      | s -> invalid_arg ("MsgType: unknown msg type " ^ s)

    let parse s =
      try Ok (parse_exn s)
      with Invalid_argument msg -> R.error_msg msg

    let to_string = function
      | Heartbeat                     -> "0"
      | TestRequest                   -> "1"
      | ResendRequest                 -> "2"
      | Reject                        -> "3"
      | SequenceReset                 -> "4"
      | Logout                        -> "5"
      | IOI                           -> "6"
      | Advertisement                 -> "7"
      | ExecutionReport               -> "8"
      | OrderCancelReject             -> "9"
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
      | NewOrderBatch                 -> "U6"
      | NewOrderBatchReject           -> "U7"
      | OrderCancelBatchRequest       -> "U4"
      | OrderCancelBatchReject        -> "U5"
  end
  include T
  include Make(T)
end

module SessionRejectReason = struct
  module T = struct
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
    [@@deriving sexp,yojson,show]


    let of_int_exn = function
      | 0 -> InvalidTag
      | 1 -> MissingTag
      | 2 -> TagNotDefinedForThisMessage
      | 3 -> UndefinedTag
      | _ -> invalid_arg "SessionRejectReason.of_int"

    let of_int i =
      try Ok (of_int_exn i)
      with Invalid_argument msg -> R.error_msg msg

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
      | None -> R.error_msg "not an int"
      | Some i -> of_int i

    let to_string t = string_of_int (to_int t)
  end
  include T
  include Make(T)
end

module ExecType = struct
  module T = struct
    type t =
      | New
      | PartialFill
      | Fill
      | DoneForDay
      | Canceled
      | Replaced
      | PendingCancel
      | Stopped
      | Rejected
      | Suspended
      | PendingNew
      | Calculated
      | Expired
      | Restated
      | PendingReplace
      | Trade
      | TradeCorrect
      | TradeCancel
      | OrderStatus
      | TradeInClearingHold
      | TradeReleasedToClearing
      | Triggered
    [@@deriving sexp,yojson,bin_io,show]

    let parse = function
      | "0" -> Ok New
      | "1" -> Ok PartialFill
      | "2" -> Ok Fill
      | "3" -> Ok DoneForDay
      | "4" -> Ok Canceled
      | "5" -> Ok Replaced
      | "6" -> Ok PendingCancel
      | "7" -> Ok Stopped
      | "8" -> Ok Rejected
      | "9" -> Ok Suspended
      | "A" -> Ok PendingNew
      | "B" -> Ok Calculated
      | "C" -> Ok Expired
      | "D" -> Ok Restated
      | "E" -> Ok PendingReplace
      | "F" -> Ok Trade
      | "G" -> Ok TradeCorrect
      | "H" -> Ok TradeCancel
      | "I" -> Ok OrderStatus
      | "J" -> Ok TradeInClearingHold
      | "K" -> Ok TradeReleasedToClearing
      | "L" -> Ok Triggered
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | New                     -> "0"
      | PartialFill             -> "1"
      | Fill                    -> "2"
      | DoneForDay              -> "3"
      | Canceled                -> "4"
      | Replaced                -> "5"
      | PendingCancel           -> "6"
      | Stopped                 -> "7"
      | Rejected                -> "8"
      | Suspended               -> "9"
      | PendingNew              -> "A"
      | Calculated              -> "B"
      | Expired                 -> "C"
      | Restated                -> "D"
      | PendingReplace          -> "E"
      | Trade                   -> "F"
      | TradeCorrect            -> "G"
      | TradeCancel             -> "H"
      | OrderStatus             -> "I"
      | TradeInClearingHold     -> "J"
      | TradeReleasedToClearing -> "K"
      | Triggered               -> "L"

  end
  include T
  include Make(T)
end

module MiscFeeType = struct
  module T = struct
    type t =
      | Regulatory
      | Tax
      | LocalCommission
      | ExchangeFees
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok Regulatory
      | "2" -> Ok Tax
      | "3" -> Ok LocalCommission
      | "4" -> Ok ExchangeFees
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | Regulatory      -> "1"
      | Tax             -> "2"
      | LocalCommission -> "3"
      | ExchangeFees    -> "4"
  end
  include T
  include Make(T)
end

module CxlRejReason = struct
  module T = struct
    type t =
      | TooLateToCancel
      | UnknownOrder
      | BrokerExchangeOption
      | PendingCancelOrReplace
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok TooLateToCancel
      | "1" -> Ok UnknownOrder
      | "2" -> Ok BrokerExchangeOption
      | "3" -> Ok PendingCancelOrReplace
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | TooLateToCancel        -> "0"
      | UnknownOrder           -> "1"
      | BrokerExchangeOption   -> "2"
      | PendingCancelOrReplace -> "3"
  end
  include T
  include Make(T)
end

module CxlRejResponseTo = struct
  module T = struct
    type t =
      | OrderCancelRequest
      | OrderReplaceRequest
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok OrderCancelRequest
      | "2" -> Ok OrderReplaceRequest
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | OrderCancelRequest  -> "1"
      | OrderReplaceRequest -> "2"
  end
  include T
  include Make(T)
end

module ExecInst = struct
  module T = struct
    type t =
      | ParticipateDoNotInitiate
      | DoNotIncrease
      | DoNotDecrease
      | AllOrNone
    [@@deriving sexp,yojson,show]

    let parse = function
      | "6" -> Ok ParticipateDoNotInitiate
      | "E" -> Ok DoNotIncrease
      | "F" -> Ok DoNotDecrease
      | "G" -> Ok AllOrNone
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | ParticipateDoNotInitiate -> "6"
      | DoNotIncrease -> "E"
      | DoNotDecrease -> "F"
      | AllOrNone -> "G"
  end
  include T
  include Make(T)
end

module CancelOrdersOnDisconnect = struct
  module T = struct
    type t =
      | All
      | Session
    [@@deriving sexp,yojson,show]

    let parse = function
      | "Y" -> Ok All
      | "S" -> Ok Session
      | _ -> R.error_msg "invalid argument"

    let to_string = function
      | All -> "Y"
      | Session -> "S"
  end
  include T
  include Make(T)
end

module LastLiquidityInd = struct
  module T = struct
    type t =
      | AddedLiquidity
      | RemovedLiquidity
      | LiquidityRoutedOut
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok AddedLiquidity
      | "2" -> Ok RemovedLiquidity
      | "3" -> Ok LiquidityRoutedOut
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | AddedLiquidity  -> "1"
      | RemovedLiquidity -> "2"
      | LiquidityRoutedOut -> "3"
  end
  include T
  include Make(T)
end

module TickDirection = struct
  module T = struct
    type t =
      | PlusTick
      | ZeroPlusTick
      | MinusTick
      | ZeroMinusTick
    [@@deriving sexp,yojson,show]

    let parse = function
      | "0" -> Ok PlusTick
      | "1" -> Ok ZeroPlusTick
      | "2" -> Ok MinusTick
      | "3" -> Ok ZeroMinusTick
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | PlusTick -> "0"
      | ZeroPlusTick -> "1"
      | MinusTick -> "2"
      | ZeroMinusTick -> "3"
  end
  include T
  include Make(T)
end

module PegPriceType = struct
  module T = struct
    type t =
      | LastPeg
      | MidPricePeg
      | OpeningPeg
      | MarketPeg
      | PrimaryPeg
      | FixedPeg
      | PegToVWAP
      | TrailingStopPeg
      | PegToLimitPrice
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok LastPeg
      | "2" -> Ok MidPricePeg
      | "3" -> Ok OpeningPeg
      | "4" -> Ok MarketPeg
      | "5" -> Ok PrimaryPeg
      | "6" -> Ok FixedPeg
      | "7" -> Ok PegToVWAP
      | "8" -> Ok TrailingStopPeg
      | "9" -> Ok PegToLimitPrice
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | LastPeg         -> "1"
      | MidPricePeg     -> "2"
      | OpeningPeg      -> "3"
      | MarketPeg       -> "4"
      | PrimaryPeg      -> "5"
      | FixedPeg        -> "6"
      | PegToVWAP       -> "7"
      | TrailingStopPeg -> "8"
      | PegToLimitPrice -> "9"
  end
  include T
  include Make(T)
end

module ContingencyType = struct
  module T = struct
    type t =
      | OCO
      | OTO
      | OUOA
      | OUOP
    [@@deriving sexp,yojson,show]

    let parse = function
      | "1" -> Ok OCO
      | "2" -> Ok OTO
      | "3" -> Ok OUOA
      | "4" -> Ok OUOP
      | _ -> R.error_msg "unknown code"

    let to_string = function
      | OCO -> "1"
      | OTO -> "2"
      | OUOA -> "3"
      | OUOP -> "4"
  end
  include T
  include Make(T)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
