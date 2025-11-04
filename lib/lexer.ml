type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

let init input =
  if String.length input == 0
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;
