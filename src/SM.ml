open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string
(* create an S-expression          *) | SEXP    of string * int
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool
(* drops the top element off       *) | DROP
(* duplicates the top element      *) | DUP
(* swaps two top elements          *) | SWAP
(* checks the tag of S-expression  *) | TAG     of string
(* enters a scope                  *) | ENTER   of string list
(* leaves a scope                  *) | LEAVE
with show
                                                   
(* The type for the stack machine program *)
type prg = insn list

let print_prg p = List.iter (fun i -> Printf.printf "%s\n" (show(insn) i)) p
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n
          
let evalCmd (prgstack, stack, (state, i, o)) cmd = match cmd with
  | BINOP op -> let (rhs :: lhs :: rest) = stack in
                let newstack = Value.of_int (Expr.to_func op (Value.to_int lhs) (Value.to_int rhs)) :: rest in
                (prgstack, newstack, (state, i, o))
  | CONST c -> (prgstack, (Value.of_int c) :: stack, (state, i, o))
  | STRING s -> (prgstack, (Value.of_string (Bytes.of_string s)) :: stack, (state, i, o))
  | SEXP (s, n) -> let (args, stack') = split n stack in (prgstack, Value.sexp s (List.rev args) :: stack', (state, i, o))
  | LD v -> (prgstack, ((State.eval state v) :: stack), (state, i, o))
  | ST v -> let (x :: rest) = stack in (prgstack, rest, ((State.update v x state), i, o))
  | STA (arr, len) -> let v::ids, newstack = split (len + 1) stack in
                      (prgstack, newstack, (Stmt.update state arr v (List.rev ids), i, o))
  | LABEL l -> (prgstack, stack, (state, i, o))
  | DROP -> let (_ :: rest) = stack in (prgstack, rest, (state, i, o))
  | DUP -> let (x :: rest) = stack in (prgstack, (x :: x :: rest), (state, i, o))
  | SWAP -> let (x :: y :: rest) = stack in (prgstack, (y :: x :: rest), (state, i, o))
  | TAG s -> let (x :: rest) = stack in let res = (match x with | Value.Sexp(s', _) when s' = s -> 1 | _ -> 0) in
              (prgstack, (Value.of_int res) :: rest, (state, i, o))
  | ENTER xs -> (prgstack, stack, (State.push state State.undefined xs, i, o))
  | LEAVE -> (prgstack, stack, (State.drop state, i, o))

let rec eval env (prgstack, stack, (state, i, o)) prog = match prog with
  | [] -> (prgstack, stack, (state, i, o))
  | (cmd :: rest) -> match cmd with
    | JMP l -> eval env (prgstack, stack, (state, i, o)) (env#labeled l)
    | CJMP (s, l) -> (let (x :: stack') = stack in match s with
                      | "nz" -> if (Value.to_int x) != 0 then eval env (prgstack, stack', (state, i, o)) (env#labeled l)
                        else eval env (prgstack, stack', (state, i, o)) rest
                      | "z" -> if (Value.to_int x) == 0 then eval env (prgstack, stack', (state, i, o)) (env#labeled l)
                        else eval env (prgstack, stack', (state, i, o)) rest)
    | BEGIN (_, args, locals) -> let add_arg x ((v :: stack), s) = (stack, State.update x v s) in
                              let (stack', state') = List.fold_right add_arg args (stack, State.enter state (args @ locals)) in
                              eval env (prgstack, stack', (state', i, o)) rest
    | END | RET _ -> (match prgstack with 
              | [] -> (prgstack, stack, (state, i, o))
              | ((prog', state') :: prgstack') -> eval env (prgstack', stack, (State.leave state state', i, o)) prog')
    | CALL (name, argn, is_ret) -> if env#is_label name then eval env ((rest, state) :: prgstack, stack, (state, i, o)) (env#labeled name)
                           else eval env (env#builtin (prgstack, stack, (state, i, o)) name argn (not is_ret)) rest
    | _ -> eval env (evalCmd (prgstack, stack, (state, i, o)) cmd) rest

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
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) (List.rev args) f in
           let stack'' = if p then stack' else let Some r = r in r::stack' in
           (*Printf.printf "Builtin:\n";*)
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec check ids exit_label = function
  | Stmt.Pattern.Wildcard -> []
  | Stmt.Pattern.Ident x -> []
  | Stmt.Pattern.Sexp (s, args) -> [DUP] @ List.flatten (List.map (fun x -> [CONST x; CALL (".elem", 2, true)]) ids) @ 
                                   [DUP; TAG s; CJMP ("z", exit_label); DUP; CALL (".length", 1, true);
                                    CONST (List.length args); BINOP ("-"); CJMP ("nz", exit_label); DROP] @
                                   List.concat(List.mapi (fun i arg -> check (ids @ [i]) exit_label arg) args)

let rec bind ids = function
  | Stmt.Pattern.Wildcard -> []
  | Stmt.Pattern.Ident _ -> [DUP] @ List.flatten (List.map (fun id -> [CONST id; CALL (".elem", 2, true)]) ids) @ [SWAP]
  | Stmt.Pattern.Sexp (s, args) -> List.concat(List.mapi (fun i arg -> bind (ids @ [i]) arg) args) @ [DROP]

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
  | Expr.String s         -> [STRING s]
  | Expr.Sexp (s, args)   -> List.concat (List.map expr args) @ [SEXP (s, List.length args)]
  | Expr.Array elems      -> List.concat (List.map expr elems) @ [CALL (".array", List.length elems, true)]
  | Expr.Elem (arr, i)    -> expr arr @ expr i @ [CALL (".elem", 2, true)]
  | Expr.Length arr       -> expr arr @ [CALL (".length", 1, true)]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (name, args) -> List.concat (List.map expr args) @ [CALL ("L" ^ name, List.length args, true)]
  in
  let rec compile_stmt stmt end_label = match stmt with
  | Stmt.Seq (s1, s2)  -> (compile_stmt s1 "") @ (compile_stmt s2 end_label)
  | Stmt.Assign (x, [], e) -> expr e @ [ST x]
  | Stmt.Assign (x, i, e) -> List.concat (List.map expr (i @ [e])) @ [STA (x, List.length i)]
  | Stmt.Skip -> []
  | Stmt.If (c, t, e) -> let else_label = name_gen#get in let cur_end_label = if end_label = "" then name_gen#get else end_label in
                           expr c @ [CJMP("z", else_label)] @ compile_stmt t cur_end_label @ [JMP cur_end_label; LABEL else_label] @
                           compile_stmt e cur_end_label @ (if end_label = "" then [LABEL cur_end_label] else [])
  | Stmt.While (c, b) -> let expr_label = name_gen#get in let body_label = name_gen#get in
                           [JMP expr_label; LABEL body_label] @ compile_stmt b "" @ [LABEL expr_label] @ 
                           expr c @ [CJMP("nz", body_label)]
  | Stmt.Repeat (b, c) -> let start_label = name_gen#get in
                            [LABEL start_label] @ compile_stmt b "" @ expr c @ [CJMP("z", start_label)]
  | Stmt.Case (v, branches) -> let cur_end_label = if end_label = "" then name_gen#get else end_label in
                                let rec compile_branch (p, s) = let next_label = name_gen#get in
                                  let s' = compile_stmt s end_label in
                                  check [] next_label p @ bind [] p @ [DROP; ENTER (List.rev (Stmt.Pattern.vars p))] @
                                  s' @ [LEAVE; JMP cur_end_label] @ (match p with Sexp _ -> [LABEL next_label; DROP] | _ -> []) in
                                expr v @ List.flatten (List.map compile_branch branches) @
                                (if end_label = "" then [LABEL cur_end_label] else [])
  | Stmt.Call (name, args) -> List.concat (List.map expr args) @ [CALL ("L" ^ name, List.length args, false)]
  | Stmt.Return x -> (match x with | None -> [RET false] | Some e -> expr e @ [RET true])
  | Stmt.Leave -> [LEAVE]
  in 
  let compile_def (name, (args, locals, body)) = [LABEL ("L" ^ name); BEGIN (name, args, locals)] @ (compile_stmt body "") @ [END]
  in
  (compile_stmt stmt "") @ [END] @ (List.concat (List.map compile_def defs))
