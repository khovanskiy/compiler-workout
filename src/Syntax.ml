(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

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

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)

    let bool2int n = if n then 1 else 0

    let int2bool n = n <> 0

    let rec eval s e = match e with
      | Const x -> x
      | Var name -> s name
      | Binop (op, lhs, rhs) ->
        let le = (eval s lhs)
        and re = (eval s rhs) in match op with
        | "+" -> le + re
        | "-" -> le - re
        | "*" -> le * re
        | "/" -> le / re
        | "%" -> le mod re
        | "!!" -> bool2int(int2bool le || int2bool re)
        | "&&" -> bool2int(int2bool le && int2bool re)
        | "==" -> bool2int(le == re)
        | "!=" -> bool2int(le != re)
        | ">" -> bool2int(le > re)
        | "<" -> bool2int(le < re)
        | ">=" -> bool2int(le >= re)
        | "<=" -> bool2int(le <= re)
        | _ -> failwith "Not implemented binary operation yet"
      | _ -> failwith "Not implemented operation yet"

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)

    let rec eval cfg stmt =
      let (s, input_list, output_list) = cfg in
      match stmt with
      | Read name -> (
          match input_list with
          | [] -> failwith "No input"
          | value :: remain -> let s' = Expr.update name value s in
            (s', remain, output_list)
        )
      | Write e -> (
          let value = Expr.eval s e in
          (s, input_list, output_list @ [value])
        )
      | Assign (name, e) -> (
          let value = Expr.eval s e in
          let s' = Expr.update name value s in
          (s', input_list, output_list)
        )
      | Seq (lhs, rhs) -> (
          let cfg' = eval cfg lhs in
          eval cfg' rhs
        )
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
