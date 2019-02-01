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

  let parse_exn = function
    | "0" -> Heartbeat
    | "1" -> TestRequest
    | "2" -> ResendRequest
    | "3" -> Reject
    | "4" -> SequenceReset
    | "5" -> Logout
    | "A" -> Logon
    | "D" -> NewOrderSingle
    | "V" -> MarketDataRequest
    | s -> invalid_arg ("MsgType: unknown msg type " ^ s)

  let parse s =
    try Some (parse_exn s) with _ -> None

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
