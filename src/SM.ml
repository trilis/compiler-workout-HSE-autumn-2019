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
(* begins procedure definition     *) | BEGIN of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL  of string * int * bool
(* returns from a function         *) | RET   of bool with show
                                                   
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
let evalCmd (prgstack, stack, (state, i, o)) cmd = match cmd with
  | BINOP op -> let (rhs :: lhs :: rest) = stack in
                let newstack = (Expr.to_func op lhs rhs) :: rest in
                (prgstack, newstack, (state, i, o))
  | CONST c -> (prgstack, c :: stack, (state, i, o))
  | READ -> let (x :: rest) = i in (prgstack, x :: stack, (state, rest, o))
  | WRITE -> let (x :: rest) = stack in (prgstack, rest, (state, i, o @ [x]))
  | LD v -> (prgstack, ((State.eval state v) :: stack), (state, i, o))
  | ST v -> let (x :: rest) = stack in (prgstack, rest, ((State.update v x state), i, o))
  | LABEL l -> (prgstack, stack, (state, i, o))

let rec eval env (prgstack, stack, (state, i, o)) prog = match prog with
  | [] -> (prgstack, stack, (state, i, o))
  | (cmd :: rest) -> match cmd with
    | JMP l -> eval env (prgstack, stack, (state, i, o)) (env#labeled l)
    | CJMP (s, l) -> (let (x :: stack') = stack in match s with
                      | "nz" -> if x != 0 then eval env (prgstack, stack', (state, i, o)) (env#labeled l)
                        else eval env (prgstack, stack', (state, i, o)) rest
                      | "z" -> if x == 0 then eval env (prgstack, stack', (state, i, o)) (env#labeled l)
                        else eval env (prgstack, stack', (state, i, o)) rest)
    | BEGIN (_, args, locals) -> let add_arg x ((v :: stack), s) = (stack, State.update x v s) in
                              let (stack', state') = List.fold_right add_arg args (stack, State.enter state (args @ locals)) in
                              eval env (prgstack, stack', (state', i, o)) rest
    | END | RET _ -> (match prgstack with 
              | [] -> (prgstack, stack, (state, i, o))
              | ((prog', state') :: prgstack') -> eval env (prgstack', stack, (State.leave state state', i, o)) prog')
    | CALL (name, _, _) -> eval env ((rest, state) :: prgstack, stack, (state, i, o)) (env#labeled name)
    | _ -> eval env (evalCmd (prgstack, stack, (state, i, o)) cmd) rest

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
let rec compile (defs, stmt) =
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
  | Expr.Call (name, args) -> List.concat (List.map expr args) @ [CALL (name, List.length args, true)]
  in
  let rec compile_stmt stmt end_label = match stmt with
  | Stmt.Seq (s1, s2)  -> (compile_stmt s1 "") @ (compile_stmt s2 end_label)
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
  | Stmt.Skip -> []
  | Stmt.If (c, t, e) -> let else_label = name_gen#get in let cur_end_label = if end_label = "" then name_gen#get else end_label in
                           expr c @ [CJMP("z", else_label)] @ compile_stmt t cur_end_label @ [JMP cur_end_label; LABEL else_label] @
                           compile_stmt e cur_end_label @ (if end_label = "" then [LABEL cur_end_label] else [])
  | Stmt.While (c, b) -> let expr_label = name_gen#get in let body_label = name_gen#get in
                           [JMP expr_label; LABEL body_label] @ compile_stmt b "" @ [LABEL expr_label] @ 
                           expr c @ [CJMP("nz", body_label)]
  | Stmt.Repeat (b, c) -> let start_label = name_gen#get in
                            [LABEL start_label] @ compile_stmt b "" @ expr c @ [CJMP("z", start_label)]
  | Stmt.Call (name, args) -> List.concat (List.map expr args) @ [CALL (name, List.length args, false)]
  | Stmt.Return x -> match x with | None -> [RET false] | Some e -> expr e @ [RET true]
  in 
  let compile_def (name, (args, locals, body)) = [LABEL name; BEGIN (name, args, locals)] @ (compile_stmt body "") @ [END]
  in
  (compile_stmt stmt "") @ [END] @ (List.concat (List.map compile_def defs))
