open Utils

(* Intermediate types for string -> regex compilation *)
type parse_unit =
    | StartGroup
    | EndGroup
    | Altern
    | Dot
    | Star
    | Plus
    | Literal of char
type parse_compound =
    | Group of parse_compound list
    | Altern of parse_compound list * parse_compound list
    | Star of parse_compound
    | Plus of parse_compound
    | Unit of parse_unit
exception CompilationException
exception TraversalException

(* Regex type *)
type regex =
    | Union of regex * regex
    | Concat of regex * regex
    | Repeat of regex * int
    | Char of char
    | Wildcard
    | Empty

(* Transition type for edges of the regex FSM *)
type transition =
    | Wildcard_T
    | Epsilon_T
    | Char_T of char

(* A lazy finite state machine type to evaluate regex expressions *)
type fsm =
    | State of (transition * (unit -> fsm) ) list
    | End
    | Null

(* Compiles a string into a regex type expression *)
let compile str =
    let parse_token = function
        | '(' -> StartGroup
        | ')' -> EndGroup
        | '|' -> Altern
        | '.' -> Dot
        | '*' -> Star
        | '+' -> Plus
        | c -> Literal(c)
    in let rec split_groups = function
        | [], 0 -> []
        | [], _ -> raise CompilationException
        | StartGroup :: xs, 0 ->  Group ( split_groups (xs, 0) ) :: split_groups (xs, 1)
        | StartGroup :: xs, k -> split_groups (xs, k + 1)
        | EndGroup :: _, 0 -> []
        | EndGroup :: xs, k when k > 0 -> split_groups (xs, k - 1)
        | x::xs, 0 -> Unit(x) :: split_groups (xs, 0)
        | _::xs, k -> split_groups (xs, k) 
    in let rec split_alterns = function
        | [], acc -> List.rev acc
        | Unit(Altern)::xs, acc -> [Altern (List.rev acc, split_alterns (xs, []))]
        | Group(l)::xs, acc -> split_alterns (xs, Group (split_alterns (l, [])) :: acc )
        | x::xs, acc -> split_alterns (xs, x::acc)
    in let rec apply_repeat = function
        | [] -> []
        | x::Unit(Star)::xs -> (match x with 
            | Group(l) -> Star(Group(apply_repeat l)) :: (apply_repeat xs)
            | Altern(l, r) -> Star(Altern(apply_repeat l, apply_repeat r)) :: (apply_repeat xs)
            | o -> Star(o) :: (apply_repeat xs))
        | x::Unit(Plus)::xs -> (match x with 
            | Group(l) -> Plus(Group(apply_repeat l)) :: (apply_repeat xs)
            | Altern(l, r) -> Plus(Altern(apply_repeat l, apply_repeat r)) :: (apply_repeat xs)
            | o -> Plus(o) :: (apply_repeat xs))
        | x::xs -> (match x with 
            | Group(l) -> Group(apply_repeat l) :: (apply_repeat xs)
            | Altern(l, r) -> Altern(apply_repeat l, apply_repeat r) :: (apply_repeat xs)
            | o -> o :: (apply_repeat xs))
    in let rec compile_to_regex = 
        let rec convert = function
            | Unit (Literal c) -> Char c
            | Unit (Dot) -> Wildcard
            | Group (l) -> compile_to_regex l
            | Altern (l, r) -> Union(compile_to_regex l, compile_to_regex r)
            | Star (l) -> Repeat (convert l, 0)
            | Plus (l) -> Repeat (convert l, 1)
            | _ -> raise CompilationException
        in function 
            | [] -> Empty
            | x::[] -> convert x
            | x::xs -> Concat(convert x, compile_to_regex xs)
    in compile_to_regex ( apply_repeat ( split_alterns (split_groups (List.map parse_token (explode str) , 0), [])))

(* Converts a regex expression into a FSM which can be traversed *)
let regex_to_nfa expr = 
    let rec regex_to_nfa_inner = function
        | Union (s, t), e -> fun () -> State ([
                (Epsilon_T, regex_to_nfa_inner (s, e)); 
                (Epsilon_T, regex_to_nfa_inner (t, e))
            ])
        | Empty, e -> fun () -> State( [(Epsilon_T, e)] )
        | Wildcard, e -> fun () -> State( [Wildcard_T, e] )
        | Char (c), e -> fun () -> State( [Char_T (c), e] )
        | Concat (s, t), e -> regex_to_nfa_inner (s, regex_to_nfa_inner (t, e))
        | Repeat (s, lb), e -> let rec start k = if k = 0 then
            State([
                (Epsilon_T, e);
                (Epsilon_T, regex_to_nfa_inner (s, fun () -> start 0) )
            ]) else 
            State([
                (Epsilon_T, regex_to_nfa_inner (s, fun () -> start (k-1)) )
            ]) in (fun () -> start lb)
    in regex_to_nfa_inner (expr, fun () -> End) ()

(* Transition functions *)
let accepts t c = match (t, c) with
    | Wildcard_T, c -> c <> '\n'
    | Char_T (a), b -> a = b
    | Epsilon_T, _  -> false

(* Traverses a regex NFA with a given string and accumulates matches *)
let traverse str nfa =
    let rec advance_epsilon (s, acc) = 
        match s with 
            | State(ts) -> List.concat (List.map 
                (function 
                    | Epsilon_T, f -> advance_epsilon (f(), acc)
                    | k -> [ (State([k]), acc) ]
                ) ts)
            | End -> [ (End, acc)]
            | Null -> []
    in let advance c (s, acc) =
        match s with 
            | State(ts) -> List.concat (List.map 
                (function 
                    | Epsilon_T, _ -> raise TraversalException
                    | t, f -> if accepts t c then [ (f (), acc ^ (Char.escaped c)) ] else [ (Null, "") ]
                ) ts)
            | End -> [ (End, acc) ]
            | Null -> raise TraversalException
    in let init_advanced = advance_epsilon (nfa, "") in
    let rec traverse_inner = function
        | [], nfas -> List.filter_map (fun (s, acc) -> if s = End then Some(acc) else None) nfas
        | c::cs, nfas -> traverse_inner ( cs, List.concat (List.map advance_epsilon (List.concat (List.map (advance c) (init_advanced @ nfas)))) )
    in traverse_inner ((explode str), init_advanced)

(* Takes a regex string and a string to match against and returns a list of matches*)
let regex_match expr str = traverse str (regex_to_nfa (compile expr))

(* Takes a regex object and a string to match against and returns a list of matches*)
let regex_eval expr str = traverse str (regex_to_nfa expr)