open List

type ident = string


type exp = Var of ident | Fun of ident * exp | App of exp * exp
           | Int of int | Bool of bool | Point of exp ref | Deref of exp
type cmd = Assign of exp * ident | Alloc of ident | Skip

type env = ident -> value option
 and value = IntVal of int | BoolVal of bool | Closure of ident * exp * env
           | InlVal of value | InrVal of value | Pointer of exp ref
type store = int -> value option

type config = cmd * env * store

let empty_env : env = fun y -> None
let empty_store : store = fun y -> None
let lookup r x : value option = r x
let lookupMem r x : config option = r x
let update r x v = fun y -> if y = x then Some v else r y
let next : int ref = ref 0
let fresh_loc (_ : unit) : int = next := !next + 1; !next

let rec eval (e : exp) (r : env) (s : store) : value option =
   match e with
   | Var x -> lookup r x
   | Int i -> Some (IntVal i)
   | Bool b -> Some (BoolVal b) 
   | Deref e -> (match eval e r s with
                     | Some (IntVal i) -> lookup s i)
   | Point p -> Some (Pointer p)

let rec step (c : cmd) (r : env) (s : store) : config option =
   match c with
   | Alloc (i) -> let l = fresh_loc () in 
      Some (Skip, update r i (IntVal l), update s l (IntVal 0) )
   | Assign (e, i) -> (match eval (Var i) r s with
                     | Some (IntVal loc) -> match eval e r s with
                                             | Some (v) -> Some (Skip, r,  (update s loc v) ) )
   | Skip -> None

let env1 : env = update empty_env "x" (IntVal 10)
let store1 : store = update empty_store 2 (IntVal 2)
let test1 = step (Alloc("x")) env1 store1(*Allocating pointer *)
let test2 = step (Assign(Int 100, ("x"))) env1 store1 (*Assigning the value of 100 to x pointer*)
let y = eval( Deref(Var "x")) env1 store1 (*Should return 100*)
