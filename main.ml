let input = "(add 2 (subtract 4 2))"

type token =
    | OpenParen
    | CloseParen
    | Number of string
    | String of string
    | Name of string

type astNode =
    | NumberLiteral of string
    | CallExpression of string * astNode list

type callee = 
    | Identifier of string

type transformedAstNode = 
    | TNumber of string
    | TCallExpression of callee * transformedAstNode list
    | TExpressionStatement of transformedAstNode

let stringToListChar s =
  let rec exp i l = if i < 0 then l else exp (i - 1) ((s.[i]) :: l) in
  exp ((String.length s) - 1) []

let tokenizer input =
    let rec tok input current tokens =
        match input with
        | [] -> List.rev tokens
        | _ ->
            let head = List.hd input in
            let tail = List.tl input in
            match (head, current, tokens) with
            (* State: No current multichar token parsing *)
            | ('(', None, t) -> tok tail None (OpenParen :: t)
            | (')', None, t) -> tok tail None (CloseParen :: t)
            | ((' ' | '\t' | '\r' | '\n'), None, t) -> tok tail None t
            | ('"', None, t) -> tok tail (Some (String (""))) t
            | (('0'..'9' as i), None, t) ->
                tok tail (Some (Number (String.make 1 i))) t
            | (('a'..'z' as i), None, t) ->
                tok tail (Some (Name (String.make 1 i))) t
            (* State: Parsing String token *)
            | ('"', (Some(String (c))),t) ->
                tok tail None ((String (c)) :: t)
            | (i,(Some (String (c))),t) ->
                tok tail (Some (String (c ^ (String.make 1 i)))) t
            (* State: Parsing Number token *)
            | (('0'..'9' as i), (Some (Number (c))),t) ->
                tok tail (Some (Number (c ^ (String.make 1 i)))) t
            | (')', (Some (Number (c))),t) ->
                tok tail None (CloseParen :: (Number (c)) :: t)
            | (' ', (Some (Number (c))),t) ->
                tok tail None ((Number (c)) :: t)
            (* State: Parsing Name token *)
            | (('a'..'z' as i), (Some ((Name (c)))),t) ->
                tok tail (Some (Name (c ^ (String.make 1 i)))) t
            | (')',(Some (Name (c))),t) ->
                tok tail None (CloseParen :: (Name (c)) :: t)
            | (' ',(Some (Name (c))),t) ->
                tok tail None ((Name (c)) :: t)
            (* State: Errors *)
            | (_,_,t) -> List.rev t in
    tok (stringToListChar input) None []

let parserFn = fun tokens ->
    let rec parse tokens stack program =
        match (tokens, stack, program) with
        (* Start of new CallExpression *)
        | (OpenParen :: Name name :: ts, s, p) -> parse ts ((CallExpression (name, [])) :: s) p
        (* When encountering a Number token, must be inside a CallExpression *)
        | (Number n :: ts, CallExpression (name, params) :: s, p) ->
                parse ts (CallExpression (name, (NumberLiteral n) :: params) :: s) p
        (* End of CallExpression when there are two CallExpression's on top of stack, need to be nested *)
        | (CloseParen :: ts, CallExpression (n, l) :: CallExpression (pn, pl) :: s, p) ->
                parse ts (CallExpression (pn, (CallExpression (n, l) :: pl)) :: s) p
        (* End of CallExpression, need to add to program *)
        | (CloseParen :: ts, ce :: s, p) -> parse ts s (ce :: p)
        (* When tokens and stack are empty, return program *)
        | ([], [], p) -> Ok p
        (* Error handling *)
        | (_, _ :: _, _) -> Error "Unmatched opened and closed parenthesis"
        | (Number _ :: _, _, _) -> Error "Unexpected number token"
        | (String _ :: _, _, _) -> Error "Unexpected string token"
        | (Name _ :: _, _, _) -> Error "Unexpected name token"
        | (OpenParen :: _, _, _) -> Error "Unexpected OpenParen token"
        | (CloseParen :: _, _, _) -> Error "Unexpected CloseParen token" in
    parse tokens [] []

let transformer = fun astList ->
  let wrapNode = fun n -> TExpressionStatement n in
  let rec transform = fun astNode ->
    match astNode with
    | CallExpression (c, l) -> 
      let nl = l |> List.map transform in
      TCallExpression ((Identifier c), nl)
    | NumberLiteral num -> TNumber num in
  astList |> (List.map transform) |> (List.map wrapNode)

let codeGenerator = fun nodes -> 
  let rec codeGenerate = fun node ->
    match node with 
    | TExpressionStatement e -> String.concat "" [codeGenerate e; ";\n"]
    | TCallExpression ((Identifier c), l) -> 
        String.concat "" [c; "("; String.concat "," (List.rev (List.map codeGenerate l)); ")"]
    | TNumber n -> n in
  List.fold_left (fun acc x -> acc ^ codeGenerate x) "" nodes

(* DEBUG *)

let tokenToString = fun token ->
    match token with
    | OpenParen -> "OpenParen"
    | CloseParen -> "CloseParen"
    | Number n -> n
    | String s -> s
    | Name n -> n


let rec astNodeToString = fun astNode ->
  match astNode with
  | NumberLiteral s -> s
  | CallExpression (s, [a; b]) -> (String.concat "" [s; "("; (astNodeToString b); ","; (astNodeToString a); ")"])
  | CallExpression (_, _) -> "This should be impossible"

(*List.iter (fun a -> Printf.printf "%s " (tokenToString a)) (tokenizer input);;*)


let () =
  match (parserFn(tokenizer input)) with
  | Ok l -> List.fold_left (fun acc x -> acc ^ astNodeToString x) "" l |> print_string
  | Error s -> print_string s

let () =
  match (parserFn(tokenizer input)) with
  | Ok l -> l |> transformer |> codeGenerator |> print_string
  | Error s -> print_string s
