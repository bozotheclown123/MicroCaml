open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks = 
  match (lookahead toks) with 
  | (Some Tok_Fun) -> parse_FunctionExpr toks

  | (Some Tok_If) -> parse_IfExpr toks

  | (Some Tok_Let) -> parse_LetExpr toks
 
  | _ -> parse_OrExpr toks

and parse_FunctionExpr toks = 
  let toks1 = match_token toks Tok_Fun in
      let iD = match (lookahead toks1) with 
          | Some Tok_ID a -> a
          | _ -> raise (InvalidInputException "invalid token option")
      in
        let toks2 = match_token toks1 (Tok_ID iD) in
          let toks3 = match_token toks2 Tok_Arrow in
            let (toks4, expr1) = parse_expr toks3 in
              (toks4, Fun (iD, expr1))

and parse_IfExpr toks =
  let toks1 = match_token toks Tok_If in
  let (toks2, expr1) = parse_expr toks1 in
  let toks3 = match_token toks2 Tok_Then in
  let (toks4, expr2) = parse_expr toks3 in
  let toks5 = match_token toks4 Tok_Else in
  let (toks6, expr3) = parse_expr toks5 in
  (toks6, If (expr1, expr2, expr3))

              
and parse_LetExpr toks =
  match lookahead toks with

  | Some Tok_Let -> begin
    let toks1 = match_token toks Tok_Let in
    match lookahead toks1 with

  | Some Tok_Rec -> begin
    let toks2 = match_token toks1 Tok_Rec in
    match lookahead toks2 with

  | Some Tok_ID id -> let toks3 = match_token toks2 (Tok_ID id) in
    let toks4 = match_token toks3 Tok_Equal in
    let (toks5, expr1) = parse_expr toks4 in
    let toks6 = match_token toks5 Tok_In in
    let (toks6, expr2) = parse_expr toks6 in (toks6, Let(id, true, expr1, expr2))
  | _ -> raise (InvalidInputException "invalid token option")
  end

  | Some Tok_ID id -> let toks2 = match_token toks1 (Tok_ID id) in
    let toks3 = match_token toks2 Tok_Equal in
    let (toks4, expr1) = parse_expr toks3 in
    let toks5 = match_token toks4 Tok_In in
    let (toks6, expr2) = parse_expr toks5 in (toks6, Let (id, false, expr1, expr2))
  | _ -> raise (InvalidInputException "invalid token option")
  end

  | _ -> raise (InvalidInputException "invalid token option")


and parse_OrExpr toks =
  let (toks1, andExpr) = parse_AndExpr toks in
  match lookahead toks1 with
  | Some Tok_Or -> let toks2 = match_token toks1 Tok_Or in
    let (toks3, orExpr) = parse_OrExpr toks2 in (toks3, Binop (Or, andExpr, orExpr))
  | _ -> (toks1, andExpr)

and parse_AndExpr toks = 
  let (toks1, equalityExpr) = parse_EqualityExpr toks in
  match lookahead toks1 with
  | Some Tok_And -> 
    let toks2 = match_token toks1 Tok_And in
    let (toks3, andExpr) = parse_AndExpr toks2 in (toks3, Binop (And, equalityExpr, andExpr))
  | _ -> (toks1, equalityExpr)

and parse_EqualityExpr toks = 
  let (toks1, relationalExpr) = parse_RelationalExpr toks in

  match lookahead toks1 with
  | Some Tok_Equal -> 
    let toks2 = match_token toks1 Tok_Equal in
    let (toks3, equalityExpr) = parse_EqualityExpr toks2 in
    (toks3, Binop (Equal, relationalExpr, equalityExpr))

  | Some Tok_NotEqual -> 
    let toks2 = match_token toks1 Tok_NotEqual in
    let (toks3, equalityExpr) = parse_EqualityExpr toks2 in
    (toks3, Binop (NotEqual, relationalExpr, equalityExpr))
    
  | _ -> 
    (toks1, relationalExpr)

and parse_RelationalExpr toks =
  let (toks1, additiveExpr) = parse_AdditiveExpr toks in
  match lookahead toks1 with

  | Some Tok_Less -> 
    let toks2 = match_token toks1 Tok_Less in
    let (toks3, relationalExpr) = parse_RelationalExpr toks2 in
    (toks3, Binop (Less, additiveExpr, relationalExpr))

  | Some Tok_LessEqual -> 
    let toks2 = match_token toks1 Tok_LessEqual in
    let (toks3, relationalExpr) = parse_RelationalExpr toks2 in
    (toks3, Binop (LessEqual, additiveExpr, relationalExpr))

  | Some Tok_Greater -> 
    let toks2 = match_token toks1 Tok_Greater in
    let (toks3, relationalExpr) = parse_RelationalExpr toks2 in
    (toks3, Binop (Greater, additiveExpr, relationalExpr))

  | Some Tok_GreaterEqual -> 
    let toks2 = match_token toks1 Tok_GreaterEqual in
    let (toks3, relationalExpr) = parse_RelationalExpr toks2 in
    (toks3, Binop (GreaterEqual, additiveExpr, relationalExpr))

  | _ -> (toks1, additiveExpr)

and parse_AdditiveExpr toks = 
  let (toks1, multiplicativeExpr) = parse_MultiplicativeExpr toks in
  match lookahead toks1 with

  | Some Tok_Add -> 
    let toks2 = match_token toks1 Tok_Add in
    let (toks3, additiveExpr) = parse_AdditiveExpr toks2 in
    (toks3, Binop (Add, multiplicativeExpr, additiveExpr))

  | Some Tok_Sub -> 
    let toks2 = match_token toks1 Tok_Sub in
    let (toks3, additiveExpr) = parse_AdditiveExpr toks2 in
    (toks3, Binop (Sub, multiplicativeExpr, additiveExpr))

  | _ -> (toks1, multiplicativeExpr)

and parse_MultiplicativeExpr toks = 
  let (toks1, concatExpr) = parse_ConcatExpr toks in
  match lookahead toks1 with
  
  | Some Tok_Mult -> 
    let toks2 = match_token toks1 Tok_Mult in
    let (toks3, multiplicativeExpr) = parse_MultiplicativeExpr toks2 in
    (toks3, Binop (Mult, concatExpr, multiplicativeExpr))

  | Some Tok_Div -> 
    let toks2 = match_token toks1 Tok_Div in
    let (toks3, multiplicativeExpr) = parse_MultiplicativeExpr toks2 in
    (toks3, Binop (Div, concatExpr, multiplicativeExpr))

  | _ -> (toks1, concatExpr)

and parse_ConcatExpr toks = 
  let (toks1, unaryExpr) = parse_UnaryExpr toks in
  match lookahead toks1 with

  | Some Tok_Concat -> 
    let toks2 = match_token toks1 Tok_Concat in
    let (toks3, concatExpr) = parse_ConcatExpr toks2 in
    (toks3, Binop (Concat, unaryExpr, concatExpr))

  | _ -> (toks1, unaryExpr)

and parse_UnaryExpr toks = 
  match lookahead toks with

  | Some Tok_Not ->
    let toks1 = match_token toks Tok_Not in
    let (toks2, unaryExpr) = parse_UnaryExpr toks1 in
    (toks2, Not (unaryExpr))

  | _ -> parse_FunctionCallExpr toks
    

and parse_FunctionCallExpr toks = 
  let (toks1, expr1) = parse_PrimaryExpr toks in
  match lookahead toks1 with

  | Some Tok_Int int -> 
    let (toks2, expr2) = parse_PrimaryExpr toks1 in
    (toks2, FunctionCall (expr1, expr2))

  | Some Tok_Bool bool -> 
    let (toks2, expr2) = parse_PrimaryExpr toks1 in
    (toks2, FunctionCall (expr1, expr2))

  | Some Tok_String string -> 
    let (toks2, expr2) = parse_PrimaryExpr toks1 in
    (toks2, FunctionCall (expr1, expr2))

  | Some Tok_ID id -> 
    let (toks2, expr2) = parse_PrimaryExpr toks1 in
    (toks2, FunctionCall (expr1, expr2))

  | Some Tok_LParen -> 
    let (toks2, expr2) = parse_PrimaryExpr toks1 in
    (toks2, FunctionCall (expr1, expr2))

  | _ -> (toks1, expr1)



and parse_PrimaryExpr toks = 
    match lookahead toks with

    | Some Tok_Int int -> 
      let toks1 = match_token toks (Tok_Int int) in (toks1, Value (Int int))

    | Some Tok_Bool bool -> 
      let toks1 = match_token toks (Tok_Bool bool) in (toks1, Value (Bool bool))

    | Some Tok_String string -> 
      let toks1 = match_token toks (Tok_String string) in (toks1, Value (String string))

    | Some Tok_ID id ->   
      let toks1 = match_token toks (Tok_ID id) in (toks1, ID id)
   
    | Some Tok_LParen -> 
      let toks1 = match_token toks Tok_LParen in
      let (toks2, expr) = parse_expr toks1 in
      let toks3 = match_token toks2 Tok_RParen in
      (toks3, expr)

    | _ -> raise (InvalidInputException "invalid token option")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with 
  | Some Tok_Def -> begin
    let toks1 = match_token toks Tok_Def in 
    match lookahead toks1 with 
    | Some Tok_ID temp -> 
      let toks1 = match_token toks1 (Tok_ID temp) in 
      let toks2 = match_token toks1 Tok_Equal in 
      let (toks3, expr) = parse_expr toks2 in 
      let toks4 = match_token toks3 Tok_DoubleSemi in (toks4, Def(temp, expr))
    | _ -> raise (InvalidInputException "invalid token option")     
    end

  | Some Tok_DoubleSemi -> let toks1 = match_token toks Tok_DoubleSemi in (toks1, NoOp)

  | _ -> let (toks1, expr) = (parse_expr toks) in 
    let toks2 = match_token toks1 Tok_DoubleSemi in (toks2, Expr(expr))
    