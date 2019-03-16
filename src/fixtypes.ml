(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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
        | ((hh, mm), None), off -> (hh, mm, 0), off
        | ((hh, mm), Some ss), off -> (hh, mm, ss), off
      end
      begin function
        | (hh, mm, 0), off -> ((hh, mm), None), off
        | (hh, mm, ss), off -> ((hh, mm), Some ss), off
      end
      (num2 <* col <&> num2 <&> opt (col *> num2) <&> tzspec)

  let time_re = Tyre.compile time

  let time_of_string s = Tyre.exec time_re s
  let time_of_string_opt s = R.to_option (time_of_string s)

  let time_of_sexp s =
    match time_of_string (string_of_sexp s) with
    | Ok time -> time
    | Error _  -> invalid_arg "time_of_sexp"

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

module UserRequestType = struct
  module T = struct
    type t =
      | Logon
      | Logoff
      | ChangePassword
      | RequestStatus
    [@@deriving sexp]

    let parse = function
      | "1" -> Some Logon
      | "2" -> Some Logoff
      | "3" -> Some ChangePassword
      | "4" -> Some RequestStatus
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "1" -> Some LoggedIn
      | "2" -> Some LoggedOff
      | "3" -> Some NotRecognized
      | "4" -> Some PasswordIncorrect
      | "5" -> Some PasswordChanged
      | "6" -> Some Other
      | "7" -> Some ForcedLogout
      | "8" -> Some SessionShutdownWarning
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "0" -> Some New
      | "1" -> Some PartiallyFilled
      | "2" -> Some Filled
      | "3" -> Some DoneForDay
      | "4" -> Some Canceled
      | "5" -> Some Replaced
      | "6" -> Some PendingCancel
      | "7" -> Some Stopped
      | "8" -> Some Rejected
      | "9" -> Some Suspended
      | "A" -> Some PendingNew
      | "B" -> Some Calculated
      | "C" -> Some Expired
      | "D" -> Some AcceptedForBidding
      | "E" -> Some PendingReplace
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Positions
      | "1" -> Some Trades
      | "2" -> Some Exercises
      | "3" -> Some Assignments
      | "4" -> Some SettlementActivity
      | "5" -> Some BackoutMessage
      | "6" -> Some DeltaPositions
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "1" -> Some Security
      | "2" -> Some UnderlyingSecurity
      | "3" -> Some Product
      | "4" -> Some CFICode
      | "5" -> Some SecurityType
      | "6" -> Some TradingSession
      | "7" -> Some AllOrders
      | "8" -> Some PartyID
      | "9" -> Some SecurityIssuer
      | "10" -> Some UssuerOfUnderlyingSecurity
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "0" -> Some ValidRequest
      | "1" -> Some InvalidRequest
      | "2" -> Some NoPositionsFound
      | "3" -> Some NotAuthorized
      | "4" -> Some Unsupported
      | _ -> None

    let print = function
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
      | MarketOnClose
      | WithOrWithout
    [@@deriving sexp]

    let parse = function
      | "1" -> Some Market
      | "2" -> Some Market
      | "3" -> Some Market
      | "4" -> Some Market
      | "5" -> Some Market
      | "6" -> Some Market
      | _ -> None

    let print = function
      | Market        -> "1"
      | Limit         -> "2"
      | Stop          -> "3"
      | StopLimit     -> "4"
      | MarketOnClose -> "5"
      | WithOrWithout -> "6"
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
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Broker
      | "1" -> Some UnknownSymbol
      | "2" -> Some ExchangeClosed
      | "3" -> Some OrderExceedsLimit
      | "4" -> Some TooLateToEnter
      | "5" -> Some UnknownOrder
      | "6" -> Some DuplicateOrder
      | _ -> None

    let print = function
      | Broker            -> "0"
      | UnknownSymbol     -> "1"
      | ExchangeClosed    -> "2"
      | OrderExceedsLimit -> "3"
      | TooLateToEnter    -> "4"
      | UnknownOrder      -> "5"
      | DuplicateOrder    -> "6"

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

module ExecTransType = struct
  module T = struct
    type t =
      | New
      | Cancel
      | Correct
      | Status
    [@@deriving sexp]

    let parse = function
      | "0" -> Some New
      | "1" -> Some Cancel
      | "2" -> Some Correct
      | "3" -> Some Status
      | _ -> None

    let print = function
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

module MDUpdateType = struct
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

module MDUpdateAction = struct
  module T = struct
    type t =
      | New
      | Change
      | Delete
      | DeleteThru
      | DeleteFrom
      | Overlay
    [@@deriving sexp]

    let parse = function
      | "0" -> Some New
      | "1" -> Some Change
      | "2" -> Some Delete
      | "3" -> Some DeleteThru
      | "4" -> Some DeleteFrom
      | "5" -> Some Overlay
      | _ -> None

    let print = function
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
      | "1" -> Some Buy
      | "2" -> Some Sell
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Session
      | "1" -> Some GoodTillCancel
      | "2" -> Some AtTheOpening
      | "3" -> Some ImmediateOrCancel
      | "4" -> Some FillOrKill
      | "5" -> Some GoodTillCrossing
      | "6" -> Some GoodTillDate
      | "7" -> Some AtTheClose
      | "8" -> Some GoodThroughCrossing
      | "9" -> Some AtCrossing
      | "P" -> Some PostOnly
      | _ -> None

    let print = function
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

module QtyType = struct
  module T = struct
    type t =
      | Units
      | Contracts
      | UnitsPerTime
    [@@deriving sexp]

    let parse = function
      | "0" -> Some Units
      | "1" -> Some Contracts
      | "2" -> Some UnitsPerTime
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "0" -> Some New
      | "1" -> Some PartialFill
      | "2" -> Some Fill
      | "3" -> Some DoneForDay
      | "4" -> Some Canceled
      | "5" -> Some Replaced
      | "6" -> Some PendingCancel
      | "7" -> Some Stopped
      | "8" -> Some Rejected
      | "9" -> Some Suspended
      | "A" -> Some PendingNew
      | "B" -> Some Calculated
      | "C" -> Some Expired
      | "D" -> Some Restated
      | "E" -> Some PendingReplace
      | "F" -> Some Trade
      | "G" -> Some TradeCorrect
      | "H" -> Some TradeCancel
      | "I" -> Some OrderStatus
      | "J" -> Some TradeInClearingHold
      | "K" -> Some TradeReleasedToClearing
      | "L" -> Some Triggered
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "1" -> Some Regulatory
      | "2" -> Some Tax
      | "3" -> Some LocalCommission
      | "4" -> Some ExchangeFees
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "0" -> Some TooLateToCancel
      | "1" -> Some UnknownOrder
      | "2" -> Some BrokerExchangeOption
      | "3" -> Some PendingCancelOrReplace
      | _ -> None

    let print = function
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
    [@@deriving sexp]

    let parse = function
      | "1" -> Some OrderCancelRequest
      | "2" -> Some OrderReplaceRequest
      | _ -> None

    let print = function
      | OrderCancelRequest  -> "1"
      | OrderReplaceRequest -> "2"
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
