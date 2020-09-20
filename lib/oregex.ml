open Utils

(* Intermediate types for string to regex compilation *)
type parse_unit =
    | StartGroup
    | EndGroup
    | Altern
    | Dot
    | Star
    | Literal of char
type parse_compound =
    | Group of parse_compound list
    | Altern of parse_compound list * parse_compound list
    | Star of parse_compound
    | Unit of parse_unit
exception CompilationException
exception TraversalException

(* Regex type *)
type regex =
    | Union of regex * regex
    | Concat of regex * regex
    | Repeat of regex
    | Char of char
    | Wildcard
    | Empty

(* Transition type for edges of the regex FSM *)
type transition =
    | Wildcard_T
    | Epsilon_T
    | Char_T of char

(* A finite state machine type to evaluate regex expression with lazily parsed transitions *)
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
    in let rec apply_star = function
        | [] -> []
        | x::Unit(Star)::xs -> (match x with 
            | Group(l) -> Star(Group(apply_star l)) :: (apply_star xs)
            | Altern(l, r) -> Star(Altern(apply_star l, apply_star r)) :: (apply_star xs)
            | o -> Star(o) :: (apply_star xs))
        | x::xs -> (match x with 
            | Group(l) -> Group(apply_star l) :: (apply_star xs)
            | Altern(l, r) -> Altern(apply_star l, apply_star r) :: (apply_star xs)
            | o -> o :: (apply_star xs))
    in let rec compile_to_regex = 
        let rec convert = function
            | Unit (Literal c) -> Char c
            | Unit (Dot) -> Wildcard
            | Group (l) -> compile_to_regex l
            | Altern (l, r) -> Union(compile_to_regex l, compile_to_regex r)
            | Star (l) -> Repeat (convert l)
            | _ -> raise CompilationException
        in function 
            | [] -> Empty
            | x::[] -> convert x
            | x::xs -> Concat(convert x, compile_to_regex xs)
    in compile_to_regex ( apply_star ( split_alterns (split_groups (List.map parse_token (explode str) , 0), [])))

(* Converts a regex expression into a FSM which can be traversed *)
let regex_to_nfa expr = 
    let rec regex_to_nfa_inner = function
        | Union (s, t), e -> fun () -> State ([
                (Epsilon_T, regex_to_nfa_inner (s, e)); 
                (Epsilon_T, regex_to_nfa_inner (t, e))
            ])
        | Empty, e -> fun () -> State( [(Epsilon_T, e)] )
        | Wildcard, e -> fun () -> State( [Wildcard_T, e] )
        | Char c, e -> fun () -> State( [Char_T (c), e] )
        | Concat (s, t), e -> regex_to_nfa_inner (s, regex_to_nfa_inner (t, e))
        | Repeat (s), e -> let rec start () = State([
                (Epsilon_T, e);
                (Epsilon_T, (regex_to_nfa_inner (s, fun () -> State([(Epsilon_T, start ); (Epsilon_T, e)]))) )
            ]) in start
    in regex_to_nfa_inner (expr, fun () -> End) ()

(* Transition functions *)
let accepts t c = match (t, c) with
    | Wildcard_T, _ -> true
    | Char_T (a), b -> a = b
    | Epsilon_T, _  -> false

(* Traverses a regex NFA with a given string to see if a match is found *)
let traverse str nfa =
    let rec advance_state c n = 
        match (c, n) with 
        | c, State (
                (Epsilon_T, f)::[]
            ) -> advance_state c (f ())
        | c, State(
                (t, f)::[]
            ) -> if accepts t c then [f ()] else [Null]
        | c, State( 
                (Epsilon_T, f)::(Epsilon_T, g)::[]
            ) -> advance_state c (f ()) @ advance_state c (g ())
        | _, End -> [End]
        | _, _ -> raise TraversalException
    in
    let rec traverse_inner = function
        | [], _ -> false
        | c::cs, nfas ->
            let advanced = List.filter (fun a -> a <> Null) (List.concat (List.map (advance_state c) (nfa::nfas)))
            in 
                if (List.exists (fun a -> a = End) advanced) then true
                else traverse_inner (cs, advanced)
    in traverse_inner ('\r'::(explode str), [])

(* Main regex match function takes a regular expression string and a string to see if the string matches *)
let regex_match expr str = traverse str (regex_to_nfa (compile expr))

(* Matches a string against a regex object *)
let regex_eval expr str = traverse str (regex_to_nfa expr)