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
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| insn :: prg' ->
  (match insn with
  | BINOP op -> let y::x::stack' = stack in eval env (cstack, Expr.to_func op x y :: stack', c) prg'
  | READ     -> let z::i'        = i     in eval env (cstack, z::stack, (st, i', o)) prg'
  | WRITE    -> let z::stack'    = stack in eval env (cstack, stack', (st, i, o @ [z])) prg'
  | CONST i  -> eval env (cstack, i::stack, c) prg'
  | LD x     -> eval env (cstack, (State.eval st x) :: stack, c) prg'
  | ST x     -> let z::stack'    = stack in eval env (cstack, stack', (State.update x z st, i, o)) prg'
  | LABEL _  -> eval env conf prg'
  | JMP l    -> eval env conf (env#labeled l)
  | CJMP (cond, l) ->
    let z::stack' = stack in
    let is_jump = match cond with
    | "nz" -> z <> 0
    | "z" -> z == 0
    in eval env (cstack, stack', c) (if is_jump then (env#labeled l) else prg')
  | BEGIN (args, locals) ->
    let rec fun_init_state state = function
      | arg::args, el::stk ->
        let n_state, n_stack = fun_init_state state (args, stk) in
        State.update arg el n_state, n_stack
      | [], stk -> state, stk
    in
    let stt, stk = fun_init_state (State.enter st (args @ locals)) (args, stack) in
    eval env (cstack, stk, (stt, i, o)) prg'
  | END -> (
    match cstack with
    | (p, stt)::cstack' -> eval env (cstack', stack, (State.leave st stt, i, o)) p
    | [] -> conf
    )
  | CALL name -> (
    eval env ((prg', st)::cstack, stack, c) (env#labeled name)
    )
  )

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (*print_prg p;*)
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
let rec compile (defs, p) = 
  let env = object
  val mutable label_ids = 0
  method next_label =
    label_ids <- (label_ids + 1);
    "l" ^ (string_of_int label_ids)
  end
  in
  let rec compile_expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> (compile_expr x) @ (compile_expr y) @ [BINOP op]
  | Expr.Call (name, args) -> List.concat (List.map compile_expr args) @ [CALL name]
  in
  let rec compile_stmt = function
  | Stmt.Seq (s1, s2)  -> (compile_stmt s1) @ (compile_stmt s2)
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> (compile_expr e) @ [WRITE]
  | Stmt.Assign (x, e) -> (compile_expr e) @ [ST x]
  | Stmt.Skip -> []
  | Stmt.If (condition, s1, s2) -> (
    let else_label = env#next_label in 
    let end_label = env#next_label in
    let s1_compiled = compile_stmt s1 in
    let s2_compiled = compile_stmt s2 in
    (compile_expr condition @ [CJMP ("z", else_label)] @ s1_compiled @ [JMP end_label] @ [LABEL else_label] @ s2_compiled @ [LABEL end_label])
  )
  | Stmt.While (condition, body) -> (
    let loop_label = env#next_label in
    let end_label = env#next_label in
    let body_compiled = compile_stmt body in
    ([JMP end_label] @ [LABEL loop_label] @ body_compiled @ [LABEL end_label] @ compile_expr condition @ [CJMP ("nz", loop_label)])
  )
  | Stmt.Repeat (body, condition) -> (
    let loop_label = env#next_label in
    let body_compiled = compile_stmt body in
    ([LABEL loop_label] @ body_compiled @ compile_expr condition @ [CJMP ("z", loop_label)])
  )
  | Stmt.Call (name, args) -> (
    let list_compiled_args = List.map compile_expr (List.rev args) in
    let compiled_args = List.concat list_compiled_args in
    compiled_args @ [CALL ("L" ^ name)]
  )
  | Stmt.Return s -> (
    match s with 
    | Some e  -> (compile_expr e) @ [END] 
    | _       -> [END]
  )
  in
  let rec compile_function (name, (args, locals, body)) = (
    [LABEL ("L" ^ name); BEGIN (args, locals)] @ compile_stmt body @ [END]
  )
  in
  ([LABEL "main"] @ compile_stmt p @ [END] @ List.concat (List.map compile_function defs))