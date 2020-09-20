open Tests

let%test _ = compilation_test_Empty ()
let%test _ = compilation_test_Wildcard ()
let%test _ = compilation_test_Char 'a'
let%test _ = compilation_test_Concat_0 ()
let%test _ = compilation_test_Concat_1 ()
let%test _ = compilation_test_Union_0 ()
let%test _ = compilation_test_Union_1 ()
let%test _ = compilation_test_Union_2 ()
let%test _ = compilation_test_Union_3 ()
let%test _ = compilation_test_Union_4 ()
let%test _ = compilation_test_Union_5 ()
let%test _ = compilation_test_Star_0 ()
let%test _ = compilation_test_Star_1 ()
let%test _ = compilation_test_Star_2 ()
let%test _ = compilation_test_Star_3 ()
let%test _ = compilation_test_Star_4 ()
let%test _ = compilation_test_Star_5 ()

let%test _ = match_test_0 ()
let%test _ = match_test_1 "k"
let%test _ = match_test_2 "f"
let%test _ = match_test_3 ()
let%test _ = match_test_4 ()
let%test _ = match_test_5 ()
let%test _ = match_test_6 ()
let%test _ = match_test_7 ()
let%test _ = match_test_8 ()
let%test _ = match_test_9 "p"
let%test _ = match_test_10 ()
let%test _ = match_test_11 ()
let%test _ = match_test_12 ()
let%test _ = match_test_13 ()
let%test _ = match_test_14 ()