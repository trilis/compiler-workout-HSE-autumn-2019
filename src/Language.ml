(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Ostap.Combinators
       
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
    let rec eval s e = match e with 
      | Const x -> x
      | Var v -> s v
      | Binop (op, l, r) -> 
        let lhs = eval s l
        and rhs = eval s r
        and b2i x = if x then 1 else 0
        and i2b x = x != 0
        in match op with
          | "+" -> lhs + rhs
          | "-" -> lhs - rhs
          | "*" -> lhs * rhs
          | "/" -> lhs / rhs
          | "%" -> lhs mod rhs
          | "<" -> b2i (lhs < rhs)
          | "<=" -> b2i (lhs <= rhs)
          | ">" -> b2i (lhs > rhs)
          | ">=" -> b2i (lhs >= rhs)
          | "==" -> b2i (lhs = rhs)
          | "!=" -> b2i (lhs <> rhs)
          | "&&" -> b2i ((i2b lhs) && (i2b rhs))
          | "!!" -> b2i ((i2b lhs) || (i2b rhs))

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)

    let ostapBinOp op = ostap ($(op)), fun x y -> Binop(op, x, y)

    ostap (
      parse:
        !(Util.expr
           (fun x -> x)
           [|
             `Lefta , [ostapBinOp "!!"];
             `Lefta , [ostapBinOp "&&"];
             `Nona , [ostapBinOp "<"; ostapBinOp "<="; ostapBinOp ">"; ostapBinOp ">="; ostapBinOp "=="; ostapBinOp "!="];
             `Lefta , [ostapBinOp "+"; ostapBinOp "-"];
             `Lefta, [ostapBinOp "*"; ostapBinOp "/"; ostapBinOp "%"];
           |]
           primary
         );
      primary: x:IDENT {Var x} | x:DECIMAL {Const x} | -"(" parse -")"
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
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (st, i, o) stmt = match stmt with 
      | Read id -> let (v :: rest) = i in (Expr.update id v st, rest, o)
      | Write e -> (st, i, o @ [(Expr.eval st e)])
      | Assign (id, e) -> (Expr.update id (Expr.eval st e) st, i, o)
      | Seq (stmt1, stmt2) -> eval (eval (st, i, o) stmt1) stmt2

    (* Statement parser *)
    ostap (
      parse: stmt | seq;
      stmt: -"read" -"(" var:IDENT -")" {Read var} 
        | -"write" -"(" expr:!(Expr.parse) -")" {Write expr}
        | var:IDENT -":=" expr:!(Expr.parse) {Assign(var, expr)};
      seq: first:stmt -";" rest:parse {Seq(first, rest)}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
