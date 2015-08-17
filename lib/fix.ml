type message =
  | Heartbeat
  | Logon
  | Logout

let msgtype_of_message = function
  | Heartbeat -> "0"
  | Logon -> "A"
  | Logout -> "5"

let message_of_msgtype = function
  | "0" -> Some Heartbeat
  | "A" -> Some Logon
  | "5" -> Some Logout
  | _ -> None

type field =
  | BeginString [@value 8]
  | BodyLength [@value 9]
  | CheckSum [@value 10]
  | SenderCompId [@value 49]
  | TargetCompId [@value 56]
  | Text [@value 58]
  | HeartBtInt [@value 108]
  | ResetSeqNumFlag [@value 141]
  | Username [@value 553]
  | Password [@value 554]
      [@@deriving enum]

type atom = {
  tag: int;
  value: string
}

let atom_of_string s =
  let i = String.index s '=' in
  { tag = int_of_string @@ String.sub s 0 i;
    value = String.sub s (i+1) (String.length s - i - 1)
  }

type msg = atom list

let msg_of_string s =
  let pos = ref 0 in
  let rec inner acc =
    try
      let i = String.index_from s !pos '\001' in
      let sub = String.sub s !pos (i - !pos) in
      inner ((atom_of_string sub) :: acc)
    with Not_found -> List.rev acc
  in inner []
