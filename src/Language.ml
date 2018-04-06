(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
open List
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let undefined x = failwith (Printf.sprintf "Variable %s is undefined" x)
    let empty = {g = undefined; l = undefined; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s = 
      let update_fun f y = if x = y then v else f y
      in 
      if List.mem x s.scope
      then { g = s.g; l = update_fun s.l; scope = s.scope }
      else { g = update_fun s.g; l = s.l; scope = s.scope }
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = if mem x s.scope then s.l x else s.g x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {g = st.g; l = undefined; scope = xs}

    (* Drops a scope *)
    let leave st st' = {g = st'.g; l = st.l; scope = st.scope}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      parse:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)
    let rec eval env ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x       -> (match i with z::i' -> ((State.update x z st), i', o) | _ -> failwith "Unexpected end of input")
      | Write   e       -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)   -> (State.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2) -> eval env (eval env conf s1) s2
      | Skip            -> conf
      | If (e, s1, s2)  -> (
        let x = Expr.eval st e in if (x != 0) then eval env conf s1 else eval env conf s2
      )
      | While (condition, body) -> (
        let rec while_loop env' ((st', _, _) as conf') = 
          let value = Expr.eval st' condition in
          if (value != 0) then while_loop env' (eval env' conf' body) else conf'
        in while_loop env conf
      )
      | Repeat (body, condition) -> (
        let rec repeat_loop env' ((st', _, _) as conf') =
          let ((st'', _, _) as conf'') = eval env' conf' body in
          let value = Expr.eval st'' condition in
          if (value == 0) then repeat_loop env' conf'' else conf''
        in repeat_loop env conf
      )
      | Call (name, exprs)  -> (
        let rec zip = function
        | x::xs, y::ys -> (x, y) :: zip (xs, ys)
        | [], []       -> []
        in
        let update_arg stat (x, e) = State.update x (Expr.eval st e) stat
        in
        let args, locals, body = env#definition name in 
        let enter_state = State.enter st @@ args @ locals in
        let init_state = List.fold_left update_arg enter_state @@ zip (args, exprs) in 
        let st', i, o = eval env (init_state, i, o) body
        in State.leave st st', i, o
      )
      | _ -> failwith "Unknown statement"
  
    let rec parse_if elif_block else_block =
      match elif_block with
      | [] -> (
        match else_block with
        | None -> Skip
        | Some action -> action
      )
      | (condition, body)::remain -> If (condition, body, parse_if remain else_block)
      
    (* Statement parser *)
    ostap (
      parse:
        s:stmt ";" ss:parse {Seq (s, ss)}
      | stmt;

      expr: !(Expr.parse);

      function_args: hd:expr tl:((-"," expr)* ) { hd :: tl } | empty { [] } ;

      stmt:
        "read" "(" x:IDENT ")"          {Read x}
      | "write" "(" e:!(Expr.parse) ")" {Write e}
      | x:IDENT ":=" e:!(Expr.parse)    {Assign (x, e)}
      | %"skip"                          {Skip}
      | %"if" condition:!(Expr.parse) %"then" body:parse
            elif_block:(%"elif" !(Expr.parse) %"then" parse)*
            else_block:(%"else" parse)?
        %"fi" {If (condition, body, (parse_if elif_block else_block))}
      | %"while" condition:!(Expr.parse) %"do" body:parse %"od" {While (condition, body)}
      | %"repeat" body:parse %"until" condition:!(Expr.parse) {Repeat (body, condition)}    
      | %"for" init:parse "," condition:!(Expr.parse) "," loop:parse %"do" body:parse %"od" {Seq (init, While (condition, Seq (body, loop)))}     
      | f:IDENT "(" args:function_args ")" { Call (f, args) }
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      parse: %"fun" name:IDENT "(" args:(ids | nothing) ")" vars:(-(%"local") ids | nothing)
        "{" body:stmt "}" { name, (args, vars, body) } ;

      stmt: !(Stmt.parse) ;

      nothing: empty { [] } ;

      ids: hd:IDENT tl:((-"," IDENT)* ) { hd :: tl }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =   
  let module FMap = Map.Make(String) 
  in
  let definitions = fold_left (fun m ((name, _) as def) -> FMap.add name def m) FMap.empty defs 
  in
  let env = (object method definition name = snd (FMap.find name definitions) end) 
  in
  let _, _, output = Stmt.eval env (State.empty, i, []) body
  in output
                                   
(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
