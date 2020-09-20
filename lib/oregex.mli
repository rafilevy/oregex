type regex =
    | Union of regex * regex
    | Concat of regex * regex
    | Repeat of regex
    | Char of char
    | Wildcard
    | Empty
val compile : string -> regex
val regex_match : string -> string -> bool
val regex_eval : regex -> string -> bool