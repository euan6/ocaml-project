x1 : t2, x2 : t2 … xn : tn

[ ] - empty int list or bool list, are not equal

If x : t and xs : tList then
X(t) :: xs(tList)

If b                        b : bool
Then e1                e1,e2 : t then whole if expression is type t
Else e2

Let g fun x -> x
Is the same as
Let g x = x

Let f = fun x -> e
X : t
E : u
F = fun x -> e		: t -> u

Let f = fun x -> if x = 5
Then “hi” (e1)
Else “bye” (e2)
SInce e1,e2 type string, f : int -> string

Let comp f g = fun x -> f(g(x))
Same as 
Let comp = fun f -> (fun g -> (fun x -> f(g(x))))

Type week = mon | tue | wed | …
This is an algebraic datatype

Let move d coords = 
Let (x,y) = coords in
Match d with
| N -> (x,y+1)
| S -> (x,y-1)
| E -> (x+1,y)
| W -> (x-1,y)

Let rec moves ds coords =
Let (x,y) = coords in
Match ds with
| [ ] -> coords
| d’ :: ds’ -> moves ds’ (move d’ (x,y))
