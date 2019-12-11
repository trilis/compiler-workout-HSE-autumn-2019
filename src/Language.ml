(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators

(* Values *)
module Value =
  struct

    @type t = Int of int | String of bytes | Array of t array | Sexp of string * t list (*with show*)

    let to_int = function 
    | Int n -> n 
    | _ -> failwith "int value expected"

    let to_string = function 
    | String s -> s 
    | _ -> failwith "string value expected"

    let to_array = function
    | Array a -> a
    | _       -> failwith "array value expected"

    let sexp   s vs = Sexp (s, vs)
    let of_int    n = Int    n
    let of_string s = String s
    let of_array  a = Array  a

    let tag_of = function
    | Sexp (t, _) -> t
    | _ -> failwith "symbolic expression expected"

    let update_string s i x = Bytes.set s i x; s 
    let update_array  a i x = a.(i) <- x; a                                          

    let string_val v =
      let buf      = Buffer.create 128 in
      let append s = Buffer.add_string buf s in
      let rec inner = function
      | Int    n    -> append (string_of_int n)
      | String s    -> append "\""; append @@ Bytes.to_string s; append "\""
      | Array  a    -> let n = Array.length a in
                       append "["; Array.iteri (fun i a -> (if i > 0 then append ", "); inner a) a; append "]"
      | Sexp (t, a) -> let n = List.length a in
                       append "`"; append t; (if n > 0 then (append " ("; List.iteri (fun i a -> (if i > 0 then append ", "); inner a) a; append ")"))
      in
      inner v;
      Bytes.of_string @@ Buffer.contents buf
                      
  end
       
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t =
    | G of (string -> Value.t)
    | L of string list * (string -> Value.t) * t

    (* Undefined state *)
    let undefined x = failwith (Printf.sprintf "Undefined variable: %s" x)

    (* Bind a variable to a value in a state *)
    let bind x v s = fun y -> if x = y then v else s y 

    (* Empty state *)
    let empty = G undefined

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let rec inner = function
      | G s -> G (bind x v s)
      | L (scope, s, enclosing) ->
         if List.mem x scope then L (scope, bind x v s, enclosing) else L (scope, s, inner enclosing)
      in
      inner s

    (* Evals a variable in a state w.r.t. a scope *)
    let rec eval s x =
      match s with
      | G s -> s x
      | L (scope, s, enclosing) -> if List.mem x scope then s x else eval enclosing x

    (* Creates a new scope, based on a given state *)
    let rec enter st xs =
      match st with
      | G _         -> L (xs, undefined, st)
      | L (_, _, e) -> enter e xs

    (* Drops a scope *)
    let leave st st' =
      let rec get = function
      | G _ as st -> st
      | L (_, _, e) -> get e
      in
      let g = get st in
      let rec recurse = function
      | L (scope, s, e) -> L (scope, s, recurse e)
      | G _             -> g
      in
      recurse st'

    (* Push a new local scope *)
    let push st s xs = L (xs, s, st)

    (* Drop a local scope *)
    let drop (L (_, _, e)) = e
                               
  end

(* Builtins *)
module Builtin =
  struct
      
    let eval (st, i, o, _) args = function
      | "read"     -> (match i with z::i' -> (st, i', o, Some (Value.of_int z)) | _ -> failwith "Unexpected end of input")
      | "write"    -> (st, i, o @ [Value.to_int @@ List.hd args], None)
      | ".elem"    -> let [b; j] = args in
                      (st, i, o, let i = Value.to_int j in
                                 Some (match b with
                                       | Value.String   s  -> Value.of_int @@ Char.code (Bytes.get s i)
                                       | Value.Array    a  -> a.(i)
                                       | Value.Sexp (_, a) -> List.nth a i
                                 )
                      )         
      | ".length"     -> (st, i, o, Some (Value.of_int (match List.hd args with Value.Sexp (_, a) -> List.length a | Value.Array a -> Array.length a | Value.String s -> Bytes.length s)))
      | ".array"      -> (st, i, o, Some (Value.of_array @@ Array.of_list args))
      | "isArray"  -> let [a] = args in (st, i, o, Some (Value.of_int @@ match a with Value.Array  _ -> 1 | _ -> 0))
      | "isString" -> let [a] = args in (st, i, o, Some (Value.of_int @@ match a with Value.String _ -> 1 | _ -> 0))                     
      | ".stringval"  -> failwith "Not implemented yet"
       
  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant   *) | Const     of int
    (* array              *) | Array     of t list
    (* string             *) | String    of string
    (* S-expressions      *) | Sexp      of string * t list
    (* variable           *) | Var       of string
    (* binary operator    *) | Binop     of string * t * t
    (* element extraction *) | Elem      of t * t
    (* length             *) | Length    of t
    (* string conversion  *) | StringVal of t
    (* function call      *) | Call      of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * Value.t option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
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
    
    let b2i x = if x then 1 else 0
    let i2b x = x != 0

    let rec eval env ((st, i, o, r) as conf) = function
      | Const n -> (st, i, o, Some (Value.of_int n))
      | Array elems -> let (st', i', o', elems') = eval_list env conf elems in env#definition env ".array" elems' (st', i', o', None)
      | String s -> (st, i, o, Some (Value.of_string (Bytes.of_string s)))
      | Sexp (s, args) -> let (st', i', o', args') = eval_list env conf args in (st', i', o', Some (Value.sexp s args'))
      | Var x -> (st, i, o, Some (State.eval st x))
      | Binop (op, x, y) -> let (_, _, _, Some x') as conf' = eval env conf x in
                            let (st', i', o', Some y') as conf'' = eval env conf' y in 
                            (st', i', o', Some (Value.of_int (to_func op (Value.to_int x') (Value.to_int y'))))
      | Elem (arr, i) -> let (st', i', o', res) = eval_list env conf [arr; i] in env#definition env ".elem" res (st', i', o', None)
      | Length arr -> let (st', i', o', Some arr') =  eval env conf arr in env#definition env ".length" [arr'] (st', i', o', None)
      | Call (name, args) -> let (st', i', o', ev_args) = eval_list env conf args in env#definition env name ev_args (st', i', o', None)
    and eval_list env conf xs =
      let vs, (st, i, o, _) =
        List.fold_left
          (fun (acc, conf) x ->
            let (_, _, _, Some v) as conf = eval env conf x in
            v::acc, conf
          )
          ([], conf)
          xs
      in
      (st, i, o, List.rev vs)


         
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
        withSuffix);

      withSuffix: 
        withElems:(arr: primary ind:(-"[" t:parse -"]")* {List.fold_left (fun acc x -> Elem (acc, x)) arr ind})
        len:("." %"length")? {match len with None -> withElems | Some _ -> Length withElems};

      primary:
        f:IDENT "(" args:!(Util.list0)[parse] ")" {Call (f, args)} 
      | n:DECIMAL {Const n}
      | c:CHAR {Const (Char.code c)}
      | s:STRING {String (String.sub s 1 (String.length s - 2))}
      | "[" elems:!(Util.list0)[parse] "]" {Array elems}
      | "`" c:IDENT "(" args:!(Util.list)[parse] ")" {Sexp (c, args)}
      | "`" c:IDENT {Sexp (c, [])}
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* Patterns in statements *)
    module Pattern =
      struct

        (* The type for patterns *)
        @type t =
        (* wildcard "-"     *) | Wildcard
        (* S-expression     *) | Sexp   of string * t list
        (* identifier       *) | Ident  of string
        with show, foldl

        (* Pattern parser *)                                 
        ostap (                                      
          parse: 
            "_" {Wildcard}    
          | "`" c:IDENT "(" args:!(Util.list)[parse] ")" {Sexp (c, args)}
          | "`" c:IDENT {Sexp (c, [])}
          | x:IDENT {Ident x}
        )

        let rec check v = function
          | Wildcard -> true
          | Ident x -> true
          | Sexp (s, args) -> (match v with 
            | Value.Sexp(s', args') -> s = s' && List.length args = List.length args' &&
              List.fold_right (fun (x, x') acc -> acc && check x' x) (List.combine args args') true
            | _ -> false
          )

        let rec branch v = function
          | [] -> None
          | (p, s)::rest -> if check v p then Some (p, s) else branch v rest

        let rec bind st = function
          | (Wildcard, _) -> st
          | (Ident x, v) -> State.bind x v st
          | (Sexp (_, args), Value.Sexp (_, args')) -> List.fold_left bind st (List.combine args args') 

        let vars p = transform(t) (fun f -> object inherit [string list, _] @t[foldl] f method c_Ident s _ name = name::s end) [] p 
        
      end
        
    (* The type for statements *)
    @type t =
    (* assignment                       *) | Assign of string * Expr.t list * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* pattern-matching                 *) | Case   of Expr.t * (Pattern.t * t) list
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list 
    (* leave a scope                    *) | Leave  with show
                                                                                   
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)
    let update st x v is =
      let rec update a v = function
      | []    -> v           
      | i::tl ->
          let i = Value.to_int i in
          (match a with
           | Value.String s when tl = [] -> Value.String (Value.update_string s i (Char.chr @@ Value.to_int v))
           | Value.Array a               -> Value.Array  (Value.update_array  a i (update a.(i) v tl))
          ) 
      in
      State.update x (match is with [] -> v | _ -> update (State.eval st x) v is) st

    let diamond s1 s2 = match s2 with
      | Skip -> s1
      | _ -> Seq(s1, s2)

    let rec eval env ((st, i, o, r) as conf) k stmt = match stmt with
      | Assign (id, ind, e) -> let (st', i', o', ind') = Expr.eval_list env conf ind in 
                               let (st', i', o', Some r) = Expr.eval env (st', i', o', None) e
                               in eval env (update st' id r ind', i', o', None) Skip k
      | Seq (stmt1, stmt2) -> eval env conf (diamond stmt2 k) stmt1
      | Skip -> (match k with | Skip -> conf | _ -> eval env conf Skip k)
      | If (cond, t, e) -> let (st', i', o', Some r') = Expr.eval env conf cond in
                           if Expr.i2b (Value.to_int r') then eval env (st', i', o', None) k t else eval env (st', i', o', None) k e
      | While (cond, body) -> let (st', i', o', Some r') = Expr.eval env conf cond in 
                              if Expr.i2b (Value.to_int r') then eval env (st', i', o', None) (diamond stmt k) body
                              else eval env (st', i', o', None) Skip k
      | Repeat (body, cond) -> eval env conf (diamond (While(Binop("==", cond, Const 0), body)) k) body
      | Case (e, cases) -> let (st', i', o', Some r) = Expr.eval env conf e in (match Pattern.branch r cases with
                              | None -> eval env (st', i', o', None) Skip k
                              | Some (p, s) -> let st'' = (State.push st' (Pattern.bind State.undefined (p, r)) (Pattern.vars p), i', o', None) in
                                  eval env st'' k s
                          )
      | Call (name, args) -> eval env (Expr.eval env conf (Expr.Call (name, args))) Skip k
      | Return x -> (match x with | None -> (st, i, o, None) | Some e -> Expr.eval env conf e)
      | Leave -> eval env (State.drop st, i, o, None) Skip k

    let orSkip x = match x with
      | Some x -> x
      | None -> Skip


    let orEmpty x = match x with
      | Some x -> x
      | None -> []
           
    (* Statement parser *)
    ostap (
      parse: seq | stmt;
      stmt: var:IDENT ind:(-"[" !(Expr.parse) -"]")* ":=" expr:!(Expr.parse) {Assign(var, ind, expr)}
        | %"skip" {Skip}
        | %"if" cond:!(Expr.parse) %"then" t:parse
            ei:(%"elif" !(Expr.parse) %"then" stmt)* e:(%"else" parse)? %"fi"
              {If(cond, t, List.fold_right (fun (cond, t') e' -> If (cond, t', e')) ei (orSkip e))}
        | %"while" cond:!(Expr.parse) %"do" body:parse %"od" {While(cond, body)}
        | %"repeat" body:parse %"until" cond:!(Expr.parse) {Repeat(body, cond)}
        | %"for" s1:parse "," e:!(Expr.parse) "," s2:parse %"do" s3:parse %"od" {Seq(s1, While(e, Seq(s3, s2)))}
        | %"case" e:!(Expr.parse) "of" cases:!(Util.listBy (ostap ("|")) branch)%"esac" {Case (e, cases)}
        | %"return" e:!(Expr.parse)? {Return e}
        | name:IDENT "(" args:(!(Util.list)[ostap(!(Expr.parse))])? ")" {Call(name, orEmpty args)};
      branch: p:!(Pattern.parse) "->" s:parse {p, Seq(s, Leave)};
      seq: first:stmt ";" rest:parse {Seq(first, rest)}
    )
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      arg: IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
         locs:(%"local" !(Util.list arg))?
        "{" body:!(Stmt.parse) "}" {
        (name, (args, (match locs with None -> [] | Some l -> l), body))
      }
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
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args ((st, i, o, r) as conf) =
           try
             let xs, locs, s      = snd @@ M.find f m in
             let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
             let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
             (State.leave st'' st, i', o', r')
           with Not_found -> Builtin.eval conf args f
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
