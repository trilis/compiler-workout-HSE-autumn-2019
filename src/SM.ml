open GT
open Syntax    
       
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
let evalCmd (stack, (state, i, o)) cmd = match cmd with
  | BINOP op -> let (rhs :: lhs :: rest) = stack in
                let newstack = (Expr.eval state (Expr.Binop (op, Expr.Const lhs, Expr.Const rhs))) :: rest in
                (newstack, (state, i, o))
  | CONST c -> (c :: stack, (state, i, o))
  | READ -> let (x :: rest) = i in (x :: stack, (state, rest, o))
  | WRITE -> let (x :: rest) = stack in (rest, (state, i, o @ [x]))
  | LD v -> ((state v) :: stack, (state, i, o))
  | ST v -> let (x :: rest) = stack in (rest, ((Expr.update v x state), i, o))

let rec eval conf prog = match prog with
  | [] -> conf
  | (cmd :: rest) -> eval (evalCmd conf cmd) rest

(* Top-level evaluation
     val run : int list -> prg -> int list
   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler
     val compile : Syntax.Stmt.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compileExpr e = match e with
  | Expr.Const x -> [CONST x]
  | Expr.Var v -> [LD v]
  | Expr.Binop (op, l, r) -> (compileExpr l) @ (compileExpr r) @ [BINOP op]


let rec compile stmt = match stmt with
  | Stmt.Read x -> READ :: [ST x]
  | Stmt.Write e -> (compileExpr e) @ [WRITE]
  | Stmt.Assign (x, e) -> (compileExpr e) @ [ST x]
  | Stmt.Seq (stmt1, stmt2) -> (compile stmt1) @ (compile stmt2)