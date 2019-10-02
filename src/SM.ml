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
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let evalCmd env (stack, (state, i, o)) cmd = match cmd with
  | BINOP op -> let (rhs :: lhs :: rest) = stack in
                let newstack = (Expr.eval state (Expr.Binop (op, Expr.Const lhs, Expr.Const rhs))) :: rest in
                (newstack, (state, i, o))
  | CONST c -> (c :: stack, (state, i, o))
  | READ -> let (x :: rest) = i in (x :: stack, (state, rest, o))
  | WRITE -> let (x :: rest) = stack in (rest, (state, i, o @ [x]))
  | LD v -> ((state v) :: stack, (state, i, o))
  | ST v -> let (x :: rest) = stack in (rest, ((Expr.update v x state), i, o))
  | LABEL l -> (stack, (state, i, o))

let rec eval env (stack, (state, i, o)) prog = match prog with
  | [] -> (stack, (state, i, o))
  | (cmd :: rest) -> match cmd with
    | JMP l -> eval env (stack, (state, i, o)) (env#labeled l)
    | CJMP (s, l) -> (let (x :: stack') = stack in match s with
                      | "nz" -> if x != 0 then eval env (stack', (state, i, o)) (env#labeled l)
                        else eval env (stack', (state, i, o)) rest
                      | "z" -> if x == 0 then eval env (stack', (state, i, o)) (env#labeled l)
                        else eval env (stack', (state, i, o)) rest)
    | _ -> eval env (evalCmd env (stack, (state, i, o)) cmd) rest

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
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let name_gen = object
    val mutable cnt = 0
    method get =
      cnt <- (cnt + 1);
      Printf.sprintf "label%d" cnt
  end in
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
  | Stmt.Skip -> []
  | Stmt.If (c, t, e) -> let else_label = name_gen#get in let end_label = name_gen#get in
                           expr c @ [CJMP("z", else_label)] @ compile t @ [JMP end_label; LABEL else_label] @
                           compile e @ [LABEL end_label]
  | Stmt.While (c, b) -> let expr_label = name_gen#get in let body_label = name_gen#get in
                           [JMP expr_label; LABEL body_label] @ compile b @ [LABEL expr_label] @ expr c @ [CJMP("nz", body_label)]
  | Stmt.Repeat (b, c) -> let start_label = name_gen#get in
                            [LABEL start_label] @ compile b @ expr c @ [CJMP("z", start_label)]
