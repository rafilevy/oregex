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
let compilation_test_Star_0 () = (compile ".*") = Repeat(Wildcard)
let compilation_test_Star_1 () = (compile "(.*)") = Repeat(Wildcard)
let compilation_test_Star_2 () = (compile "a.*") = Concat(Char('a'), Repeat(Wildcard))
let compilation_test_Star_3 () = (compile "(.a)*") = Repeat(Concat(Wildcard, Char('a')))
let compilation_test_Star_4 () = (compile ".|(a*)") = Union(Wildcard, Repeat(Char('a')))
let compilation_test_Star_5 () = (compile "(a|.)*") = Repeat(Union(Char('a'), Wildcard))

let match_test_0 () = regex_match "" ""
let match_test_1 a = regex_match "." a
let match_test_2 a = regex_match a a
let match_test_3 () = regex_match "ab" "ab"
let match_test_4 () = regex_match "a|b" "a"
let match_test_5 () = regex_match "ab|cd|ef" "ab"
let match_test_6 () = regex_match "ab|cd|ef" "cd"
let match_test_7 () = regex_match "ab|cd|ef" "ef"
let match_test_8 () = regex_match "a.a" "afa"
let match_test_9 a = regex_match "a.a" ("a"^a^"a")
let match_test_10 () = regex_match "a*" ""
let match_test_11 () = regex_match "a*" "a"
let match_test_12 () = regex_match "a*" "aa"
let match_test_13 () = regex_match "a*" "aaaaaaaaaaaaaaaa"
let match_test_14 () = regex_match "(a|b)*" "abba"