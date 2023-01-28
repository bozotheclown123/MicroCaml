open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
    | Value value -> value

    | ID id -> (lookup env id)

    | Not expr -> begin
      match (eval_expr env expr) with 
      | Bool b -> Bool (not b)
      | _ ->  raise (TypeError ("Expected type bool"))
      end

    | Binop (operator, expr1, expr2) -> begin 
        match (eval_expr env expr1) with
        | Int num1 -> begin
          match (eval_expr env expr2) with
          | Int num2 -> begin
            match operator with
            | Add -> Int (num1 + num2)
            | Sub -> Int (num1 - num2)
            | Mult -> Int (num1 * num2)
            | Div -> if num2 = 0 then raise (DivByZeroError) else Int (num1 / num2)
            | Greater -> Bool (num1 > num2)
            | Less -> Bool (num1 < num2)
            | GreaterEqual -> Bool (num1 >= num2)
            | LessEqual -> Bool (num1 <= num2)
            | Equal -> Bool (num1 = num2)
            | NotEqual -> Bool (num1 <> num2)
            | _ ->  raise (TypeError ("Invalid operator"))
          end
          | _ -> raise (TypeError ("Expected type int"))
        end

        | String string1 -> begin 
          match (eval_expr env expr2) with
          | String string2 -> begin 
            match operator with
            | Concat -> String (string1 ^ string2)
            | Equal -> Bool (string1 = string2)
            | NotEqual -> Bool (string1 <> string2)
            | _ ->  raise (TypeError ("Invalid operator"))
          end
          | _ -> raise (TypeError ("Expected type string"))
        end
                          
        | Bool bool1 -> begin
          match (eval_expr env expr2) with
          | Bool bool2 -> begin
            match operator with
            | Equal -> Bool (bool1 = bool2)
            | NotEqual -> Bool (bool1 <> bool2)
            | Or -> Bool (bool1 || bool2)
            | And -> Bool (bool1 && bool2)
            | _ ->  raise (TypeError ("Invalid operator"))
          end 
          | _ -> raise (TypeError ("Expected type bool"))
        end

        | _ ->  raise (TypeError ("Expected int/bool/string type"))
        (* not an int/bool/string type for the binop case*)
      end
  
    | Fun (var, expr) -> Closure (env, var, expr) 

    | If (guard, true_branch, false_branch) -> begin
      match eval_expr env guard with
      | Bool true ->  (eval_expr env true_branch) 
      | Bool false -> (eval_expr env false_branch)
      | _ -> raise (TypeError ("Expected type bool"))
    end

    (* false is non-recursive *)
    | Let (var, false, initilization, body) -> 
      let v = (eval_expr env initilization) in eval_expr (extend env var v) body

    (* true is recursive *)
    | Let (var, true, initilization, body) ->  
      let temp = (extend_tmp env var) in let v = (eval_expr temp initilization) in (update temp var v);
      eval_expr temp body

    | FunctionCall (expr1, expr2) -> 
      match eval_expr env expr1 with 
       | Closure (environment, x, e) -> 
           let newEnv  = extend environment x (eval_expr env expr2) in 
           eval_expr newEnv e
       | _ -> raise (TypeError "Expected type Closure")
      
         

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
    | Def (var, expr) -> 
      let temp = (extend_tmp env var) in let v = (eval_expr temp expr) in (update temp var v);
      (temp, Some v)

    | Expr (expr) -> (env, Some (eval_expr env expr))

    | NoOp -> ([], None)
