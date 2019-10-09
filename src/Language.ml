(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Ostap.Combinators

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

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

    let b2i x = if x then 1 else 0
    let i2b x = x != 0


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

       which returns a list of formal parameters and a body for given definition
    *)
    let rec eval env (st, i, o) stmt = match stmt with 
      | Read id -> let (v :: rest) = i in (State.update id v st, rest, o)
      | Write e -> (st, i, o @ [(Expr.eval st e)])
      | Assign (id, e) -> (State.update id (Expr.eval st e) st, i, o)
      | Seq (stmt1, stmt2) -> eval env (eval env (st, i, o) stmt1) stmt2
      | Skip -> (st, i, o)
      | If (cond, t, e) -> if Expr.i2b(Expr.eval st cond) then eval env (st, i, o) t else eval env (st, i, o) e
      | While (cond, body) -> if Expr.i2b(Expr.eval st cond) then eval env (eval env (st, i, o) body) stmt else (st, i, o)
      | Repeat (body, cond) -> let res = eval env (st, i, o) body in eval env res (While(Binop("==", cond, Const 0), body))
      | Call (name, args) -> let (arg_names, locals, body) = env#definition name in
                             let eval_args = List.map (Expr.eval st) args in
                             let new_state = List.fold_left2 (fun s x v -> State.update x v s) 
                               (State.push_scope st (arg_names @ locals)) arg_names eval_args in
                             let (st', i', o') = eval env (new_state, i, o) body in
                             (State.drop_scope st' st, i', o')

                                
    (* Statement parser *)
    
    let orSkip x = match x with
      | Some x -> x
      | None -> Skip


    let orEmpty x = match x with
      | Some x -> x
      | None -> []


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
        | "for" s1:parse "," e:!(Expr.parse) "," s2:parse "do" s3:parse "od" {Seq(s1, While(e, Seq(s3, s2)))}
        | name:IDENT "(" args:(!(Util.list)[ostap(!(Expr.parse))])? ")" {Call(name, orEmpty args)};
      seq: first:stmt ";" rest:parse {Seq(first, rest)}
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      parse: "fun" name:IDENT "(" args:!(Util.list)[ostap(IDENT)]? ")" 
         local_vars:(-"local" !(Util.list)[ostap(IDENT)])? "{" body:!(Stmt.parse) "}" {(name, (Stmt.orEmpty args, Stmt.orEmpty local_vars, body))}
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
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
