(* Splits a string into a list of characters *)
let explode s = List.init (String.length s) (String.get s)