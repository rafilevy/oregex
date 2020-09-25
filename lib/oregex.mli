type regex =
    | Union of regex * regex
    | Concat of regex * regex
    | Repeat of regex * int
    | Char of char
    | Wildcard
    | Empty
val compile : string -> regex
val regex_match : string -> string -> string list
val regex_eval : regex -> string -> string list