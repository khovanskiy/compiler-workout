open GT
open Language

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let rec eval cfg p =
  let (stack, cfg') = cfg in
  let (s, input_list, output_list) = cfg' in match p with
  | [] -> cfg
  | record :: remain -> match record with
    | CONST x -> eval (x :: stack, cfg') remain
    | LD name -> let value = s name in
      eval (value :: stack, cfg') remain
    | ST name -> (match stack with
        | [] -> failwith "Empty stack"
        | value :: stack' ->
          let s' = Syntax.Expr.update name value s in
          let cfg'' = (s', input_list, output_list) in
          eval (stack', cfg'') remain
      )
    | READ -> (
        match input_list with
        | value :: xs ->
          let cfg'' = (s, xs, output_list) in
          eval (value :: stack, cfg'') remain
        | _ -> failwith "No input"
      )
    | WRITE -> (
        match stack with
        | [] -> failwith "Empty stack"
        | value :: stack' ->
          let cfg'' = (s, input_list, output_list @ [value]) in
          eval (stack', cfg'') remain
      )
    | BINOP op -> (
        match stack with
        | [] -> failwith "Empty stack"
        | b :: a :: stack' ->
          let ast = Syntax.Expr.Binop op (Syntax.Expr.Const a) (Syntax.Expr.Const b) in
          let value = Syntax.Expr.eval s ast in
          eval (value :: stack', cfg') remain
      )
    | _ -> failwith "Not implemented yet"

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile_expr : Syntax.Expr.e -> prg

*)
let rec compile_expr e = match e with
  | Syntax.Expr.Const x -> [CONST x]
  | Syntax.Expr.Var name -> [LD name]
  | Syntax.Expr.Binop (op, lhs, rhs) ->
    let le = (compile_expr lhs) in
    let re = (compile_expr rhs) in
    le @ re @ [BINOP op]
  | _ -> failwith "Not implemented yet"

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile stmt = match stmt with
  | Syntax.Stmt.Read name -> [READ] @ [ST name]
  | Syntax.Stmt.Write e -> (compile_expr e) @ [WRITE]
  | Syntax.Stmt.Assign (name, e) -> (compile_expr e) @ [ST name]
  | Syntax.Stmt.Seq (lhs, rhs) -> (compile lhs) @ (compile rhs)
  | _ -> failwith "Not implemented yet"
