[5; 3; 1];;
int list = [5; 3; 1]

[3; true];;
Type error

3 :: 4 :: [ ];;
Int list = [3; 4]

Let xs = 4 :: [ ];;
Val xs int list [4]

3 :: xs;;
Int list = [3; 4]

Let mulX p = 0 :: p;;
mulX [5; 3; 1];;
Int list = [0; 5; 3; 1]

Let isPoly p =
Let rec helper p b =
	Match p with
	| [ ] -> not b
	| a :: p1 ->
	If a = 0 then helper p1 true
	Else helper p1 false
In helper p false;;

Let xs = [3];;
(3 + 2) :: xs;;
Int list [5; 3]

Let rec length p = 
Match p with
| [ ] -> 0
| a :: p -> 1 + length p

Let degree p = 
In match p with
| [ ] -> 0
| a :: p -> length (a :: p) - 1

Let rec append xs ys = 
Match xs with
| [ ] -> ys
| x :: xs -> x :: append xs ys;;
—
Let result =
Append [4;3] [8;9]
—
Let result2 =
Append result [1;2]
