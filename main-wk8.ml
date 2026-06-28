Type ‘a tree = Node of ‘a * ‘a tree * ‘a tree | Leaf

Type aexp = Num of int | Neg of aexp | Add of aexp * aexp | Mul of aexp * aexp

Let ex1 = Add (Num 3, Mul (Num 4, Num 5))
Let ex2 = Mul (Add (Num 3, Num 4), Num 5)
Let ex3 = Add (Num3, Neg (Mul (Num 4, Num5)))

Let paren s = “(“ ^ s ^ “)”

Let rec printAexp ae =
Match ae with
| Num n -> Int.to_string n
| Ned ae -> “- “ ^ printAexp ae
| Add (ae1, ae2) -> parens (printAexp ae1 ^ “ + “ ^ printAexp ae2)
| Mul (ae1, ae2) -> parens (printAexp ae1 ^ “ * “ ^ printAexp ae2)

Let rec evalAexp ae =
Match ae with
| Num n -> n
| Neg ae -> - (evalAexp ae)
| Add (ae1, ae2) -> evalAexp ae1 + evalAexp ae2
| Mul (ae1, ae2) -> evalAexp ae1 * evalAexp ae2

Type elt = Par of string | Sec of string * content 
And content = Empty | Elt of elt * content

Let rec tocE e = 
Match e with
| Par s ->
| Sec (t, ct) -> t :: tocC ct

And tocC ct =
Match ct with
| Empty -> [ ]
| Elt (e, ct) -> append (tocE e) (tocC ct)
