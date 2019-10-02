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
    let b2i x = if x then 1 else 0
    let i2b x = x != 0

    let rec eval s e = match e with 
      | Const x -> x
      | Var v -> s v
      | Binop (op, l, r) -> 
        let lhs = eval s l
        and rhs = eval s r
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
    ostap (                                      
      parse:
        !(Util.expr
           (fun x -> x)
           [|
            `Lefta , [ostap ("!!"), fun x y -> Binop ("!!", x, y)];
            `Lefta , [ostap ("&&"), fun x y -> Binop ("&&", x, y)]; 
            `Nona , [ostap ("=="), (fun x y -> Binop ("==", x, y));
                     ostap ("!="), (fun x y -> Binop ("!=", x, y));
                     ostap ("<="), (fun x y -> Binop ("<=", x, y));
                     ostap ("<"), (fun x y -> Binop ("<", x, y));
                     ostap (">="), (fun x y -> Binop (">=", x, y));
                     ostap (">"), (fun x y -> Binop (">", x, y))]; 
            `Lefta , [ostap ("+"), (fun x y -> Binop ("+", x, y));
                      ostap ("-"), (fun x y -> Binop ("-", x, y))]; 
            `Lefta , [ostap ("*"), (fun x y -> Binop ("*", x, y));
                      ostap ("/"), (fun x y -> Binop ("/", x, y));
                      ostap ("%"), (fun x y -> Binop ("%", x, y))]
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
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t with show

                                                                    
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
      | Skip -> (st, i, o)
      | If (cond, t, e) -> if Expr.i2b(Expr.eval st cond) then eval (st, i, o) t else eval (st, i, o) e
      | While (cond, body) -> if Expr.i2b(Expr.eval st cond) then eval (eval (st, i, o) body) stmt else (st, i, o)
      | Repeat (body, cond) -> let res = eval (st, i, o) body in eval res (While(Binop("==", cond, Const 0), body))

                               
    (* Statement parser *)
    let orSkip x = match x with
      | Some x -> x
      | None -> Skip

    ostap (
      parse: seq | stmt;
      stmt: "read" "(" var:IDENT ")" {Read var} 
        | "write" "(" expr:!(Expr.parse) ")" {Write expr}
        | var:IDENT ":=" expr:!(Expr.parse) {Assign(var, expr)}
        | "skip" {Skip}
        | "if" cond:!(Expr.parse) "then" t:parse
            ei:(-"elif" !(Expr.parse) -"then" stmt)* e:(-"else" parse)? "fi"
              {If(cond, t, List.fold_right (fun (cond, t') e' -> If (cond, t', e')) ei (orSkip e))}
        | "while" cond:!(Expr.parse) "do" body:parse "od" {While(cond, body)}
        | "repeat" body:parse "until" cond:!(Expr.parse) {Repeat(body, cond)}
        | "for" s1:parse "," e:!(Expr.parse) "," s2:parse "do" s3:parse "od" {Seq(s1, While(e, Seq(s3, s2)))};
      seq: first:stmt ";" rest:parse {Seq(first, rest)}
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

(* Top-level parser *)
let parse = Stmt.parse                                                     
