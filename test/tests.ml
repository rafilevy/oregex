open Oregex
let compilation_test_Empty () = (compile "") = Empty
let compilation_test_Wildcard () = (compile ".") = Wildcard
let compilation_test_Char c = (compile (Char.escaped c)) = Char(c)
let compilation_test_Concat_0 () = (compile "ab.d") = Concat(Char('a'), Concat(Char('b'), Concat(Wildcard, Char('d'))))
let compilation_test_Concat_1 () = (compile "a(b)((.d))") = Concat(Char('a'), Concat(Char('b'), Concat(Wildcard, Char('d'))))
let compilation_test_Union_0 () = (compile "a|b") = Union(Char('a'), Char('b'))
let compilation_test_Union_1 () = (compile "ab|cd") = Union(Concat(Char('a'), Char('b')), Concat(Char('c'), Char('d'))) 
let compilation_test_Union_2 () = (compile "a|(b|c)") = Union(Char('a'), Union(Char('b'), Char('c'))) 
let compilation_test_Union_3 () = (compile "a|b|cd") = Union(Char('a'), Union(Char('b'), Concat(Char('c'), Char('d'))))
let compilation_test_Union_4 () = (compile "|.") = Union(Empty, Wildcard)
let compilation_test_Union_5 () = (compile ".|") = Union(Wildcard, Empty)
let compilation_test_Star_0 () = (compile ".*") = Repeat(Wildcard, 0)
let compilation_test_Star_1 () = (compile "(.*)") = Repeat(Wildcard, 0)
let compilation_test_Star_2 () = (compile "a.*") = Concat(Char('a'), Repeat(Wildcard, 0))
let compilation_test_Star_3 () = (compile "(.a)*") = Repeat(Concat(Wildcard, Char('a')), 0)
let compilation_test_Star_4 () = (compile ".|(a*)") = Union(Wildcard, Repeat(Char('a'), 0))
let compilation_test_Star_5 () = (compile "(a|.)*") = Repeat(Union(Char('a'), Wildcard), 0)


let compilation_test_Plus_0 () = (compile ".+") = Repeat(Wildcard, 1)
let compilation_test_Plus_1 () = (compile "(.+)") = Repeat(Wildcard, 1)
let compilation_test_Plus_2 () = (compile "a.+") = Concat(Char('a'), Repeat(Wildcard, 1))
let compilation_test_Plus_3 () = (compile "(.a)+") = Repeat(Concat(Wildcard, Char('a')), 1)
let compilation_test_Plus_4 () = (compile ".|(a+)") = Union(Wildcard, Repeat(Char('a'), 1))
let compilation_test_Plus_5 () = (compile "(a|.)+") = Repeat(Union(Char('a'), Wildcard), 1)

let match_exists r s = List.length (regex_match r s) <> 0

let match_test_0 () = match_exists "" ""
let match_test_1 a = match_exists "." a
let match_test_2 a = match_exists a a
let match_test_3 () = match_exists "ab" "ab"
let match_test_4 () = match_exists "a|b" "a"
let match_test_5 () = match_exists "ab|cd|ef" "ab"
let match_test_6 () = match_exists "ab|cd|ef" "cd"
let match_test_7 () = match_exists "ab|cd|ef" "ef"
let match_test_8 () = match_exists "a.a" "afa"
let match_test_9 a = match_exists "a.a" ("a"^a^"a")
let match_test_10 () = match_exists "a*" ""
let match_test_11 () = match_exists "a*" "a"
let match_test_12 () = match_exists "a*" "aaa"
let match_test_13 () = match_exists "ca*b" "cb"
let match_test_14 () = match_exists "(a|b)*" "abba"
let match_test_15 () = not (match_exists "a+" "")
let match_test_16 () = match_exists "a+" "a"
let match_test_17 () = match_exists "a+b" "aab"
let match_test_18 () = match_exists "a*b" "b"
let match_test_19 () = match_exists "a+b" "aab"
let match_test_20 () = match_exists "(a|b|c)+.*de" "abbbccfffde"
let match_test_21 () = not (match_exists "(a|b|c)+.*de" "fffde")
let match_test_22 () = (match_exists "(a|b|c)+.*de" "ababbccde")
let match_test_23 () = not (match_exists "(a|b|c)+.*de" "abbbccfffd")