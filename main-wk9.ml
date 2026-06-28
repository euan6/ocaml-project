Type var = string
Type fname = string

Type expr = Num of int | Bool of bool | Var of var | App of fname * expr list | 
        If of expr * expr * expr

Type tl = Expr of expr | Bind of var * expr | FunD of fname * var list * expr

Open Syntax

Type typ = TNum | TBool
Type value = VNum of int | VBool of bool

Type env = (var * value) list

Type fdescr = Builtin | UDef of fname * fenv * env * var list * expr
And fenv = (fnam * fdescr) list

Exception UndefinedVariable of var

Let rec lookup env x =
Match env with
| [ ] -> raise (UndefinedVariable x)
| (y, v) :: env -> if x = y then v else lookup env x

Exception TypeError of typ * value
Exception WrongArgNumber

Let rec mkEnv xs vs env =
Match xs, vs with
| [ ], [ ] -> env 
| x :: xs, v :: vs ->
	mkEnv xs vs ((x, v) :: env)
| [ ], _ :: 

Let rec eval env fenv e = 
Match e with
| Num n -> VNum n
| Bool b -> VBool b
| Var x -> lookup env x
| App (f, es) -> 
	Let vs = List.map (eval env fenv) es
	In call f (lookup fenv f) vs
| If (e, et, ef) -> 
match eval env fenv e with
	| VBool true -> eval fenv env et
	| VBool false -> eval fenv env ef
	| v -> raise (TypeError (TBool, v))

Let call f fd vs =
Match fd with
| Builtin -> callBi f vs
| UDef (f, fenv, env, xs, e) -> 
eval (mkEnv xs vs env) ((f, fd) :: fenv) e

Let callBi f vs = 
Match f, vs with
| “add”, [VNum m; VNum n] -> VNum (m + n)
| “sub”, [VNum m; VNum n] -> VNum (m - n)
| “mul”, [VNum m; VNum n] -> VNum (m * n)
| “div”, [VNum m; VNum n] -> VNum (m / n)
| “mod”, [VNum m; VNum n] -> VNum (m mod n)
| “eq”, [VNum m; VNum n] -> VBool (m = n)
| “lt”, [VNum m; VNum n] -> VBool (m < n)
| “gt”, [VNum m; VNum n] -> VBool (m > n)
