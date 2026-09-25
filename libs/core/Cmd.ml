(* claude: see Cmd.mli *)

type http_error =
  | Bad_url of string
  | Timeout
  | Network_error of string
  | Bad_status of int
  | Bad_body of string

type http_response = { url : string; status : int; headers : (string * string) list; body : string }

type 'msg t =
  | None
  | Msg of 'msg
  | Http_get of Cap.network * string * ((http_response, http_error) result -> 'msg)
  | Batch of 'msg t list

let none = None
let batch cmds = Batch cmds

let rec to_list (cmd : 'msg t) : 'msg t list =
  match cmd with
  | None -> []
  | Batch cmds -> List.concat_map to_list cmds
  | Msg _ | Http_get _ -> [ cmd ]
