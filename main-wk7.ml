Type boolList = Empty
| Cons of bool * boolList

Let rec length xs =
Match xs with
| Empty -> 0
| Cons (_,ys) -> 1 + length ys

Type intList = Empty
| Cons of int * intList

Type ‘a myList = E
| C of ‘a * ‘a myList

Binary Trees

Type ‘a tree = Lf 
| Br of ‘a * ‘a tree * ‘a tree

Let t1 = Br ( true, Lf, Lf);;
Let t2 = Br ( false, Lf, Lf);;

Let rec size tree = 
Match tree with
| Lf -> 0
| Br (v, t1, t2) -> 1 + size t1 + size t2

Let rec depth tree =
Match tree with
| Lf -> 0
| Br (v, t1, t2) -> 1 + max (depth t1) (depth t2)

Let rec append xs ys = 
Match xs with
| [] -> ys
| (x::xs) -> x::(append xs ys)

Let rec dft tree = 
Match tree with
| Lf -> []
| Br (v, t1, t2) -> v::append (dft t1) (dft t2)
Let rec flatten tree =
Match tree with
| Lf -> []
| Br (v, t1, t2) -> append (flatten t1) (v::flatten t2)
