open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)

let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| inst :: prg' -> 
  (
    match inst with
    | BINOP op            -> let y::x::stack'   = stack in eval env (cstack, (Expr.to_func op x y) :: stack', c) prg'
    | READ                -> let z::i'          = i in eval env (cstack, z::stack, (st, i', o)) prg'
    | WRITE               -> let z::stack'      = stack in eval env (cstack, stack', (st, i, o @ [z])) prg'
    | CONST i             -> eval env (cstack, i::stack, c) prg'
    | LD x                -> eval env (cstack, (State.eval st x) :: stack, c) prg'
    | ST x                -> let z::stack'      = stack in eval env (cstack, stack', (State.update x z st, i, o)) prg'
    | LABEL l             -> eval env conf prg'
    | JMP label           -> eval env conf (env#labeled label)
    | CJMP (cond, label)  -> 
      let z::stack'      = stack in 
      let x = match cond with
      | "nz" -> z <> 0
      | "z" -> z = 0 
      in eval env (cstack, stack', c) (if (x) then (env#labeled label) else prg')
    | BEGIN (args, xs) -> (
      let rec init_args state = function
        | a::args, z::stack -> 
          let state', stack' = init_args state (args, stack)
          in State.update a z state', stack'
        | [], stack -> state, stack
      in let st', stack' = init_args (State.enter st @@ args @ xs) (args, stack)
      in eval env (cstack, stack', (st', i, o)) prg'
    )
    | END -> (
        match conf with
        | (program, s)::cstack', stack, (s', i, o) ->
          eval env (cstack', stack, (State.leave s s', i, o)) program
        | _ -> conf
    )
    | CALL f -> let cstack, stack, ((s, i, o) as conf) = conf
      in eval env ((prg', s)::cstack, stack, conf) @@ env#labeled f
    | _ -> failwith "Unsupported instruction"
  )

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let compile (defs, prg) =
  let env = object 
      val mutable id = 0
      method next_label = 
        id <- (id + 1);
        "l" ^ string_of_int id
  end
  in
  let rec expr = function
    | Expr.Var   x          -> [LD x]
    | Expr.Const n          -> [CONST n]
    | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
    | _ -> failwith "Unknown expression"
  in
  let rec compile' = function
    | Stmt.Seq (s1, s2)  -> compile' s1 @ compile' s2
    | Stmt.Read x        -> [READ; ST x]
    | Stmt.Write e       -> expr e @ [WRITE]
    | Stmt.Assign (x, e) -> expr e @ [ST x]
    | Stmt.Skip          -> []
    | Stmt.If (e, s1, s2)-> (
        let else_label = env#next_label in
        let end_label = env#next_label in
        let current_case = compile' s1 in
        let last_case = compile' s2 in
        (expr e @ [CJMP ("z", else_label)] @ current_case @ [JMP end_label] @ [LABEL else_label] @ last_case @ [LABEL end_label])
    )
    | Stmt.While (e, s)  -> (
        let end_label = env#next_label in
        let loop_label = env#next_label in
        let body = compile' s in
        ([JMP end_label] @ [LABEL loop_label] @ body @ [LABEL end_label] @ expr e @ [CJMP ("nz", loop_label)])
    )
    | Stmt.Repeat (s, e) -> (
        let loop_label = env#next_label in
        let body = compile' s in
        ([LABEL loop_label] @ body @ expr e @ [CJMP ("z", loop_label)])
    )
    | Stmt.Call (name, args) -> (
      let compiled_args_list = List.map expr (List.rev args) in
      let compiled_args = List.concat compiled_args_list in
      compiled_args @ [CALL ("L" ^ name)]
    )
  in
  let rec compile_function (name, (args, locals, body)) =
    let end_label = env#next_label in
    let compiled_body = compile' body in
    ([LABEL ("L" ^ name); BEGIN(args, locals)] @ compiled_body @ [LABEL end_label; END])
  in
  let compile_functions functions = 
    List.fold_left 
    (
      fun (functions') (name, signature) -> 
      let compiled_function = compile_function (name, signature) 
      in 
      compiled_function :: functions'
    )
    ([]) functions
  in
  let end_label = env#next_label in
  let compiled_main_function = compile' prg in 
  let compiled_functions     = compile_functions defs in
  (LABEL "main" :: compiled_main_function @ [LABEL end_label]) @ [END] @ (List.concat compiled_functions)