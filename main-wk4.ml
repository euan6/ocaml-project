Let rec append xs ys =
Match xs with
| [ ] -> ys
| x :: xs -> x :: append xs ys;;

Let rec quicksort xs = 
Match xs with
| [ ] -> [ ]
| x :: xs ->
Let small = keepSmaller x xs in
Let large = keepLarger x xs in
Let ssmall = quicksort small in
Let slarge = quicksort large in
Append ssmall (x :: slarge);;

Let rec keepSmaller p xs =
Filter ((>) p) xs;;
—
Let lessThanP x = x < p in
filter lessThanP xs;;
—
Match xs with
| [ ] -> [ ]
| x :: xs ->
If x < p then x :: keepSmaller p xs
Else keepSmaller p xs;;

Let rec keepLarger p xs =
Filter ((>=) p) xs;;
—
Let greaterThanP x = x >= p in
filter greaterThanP xs;;
—
Match xs with 
| [ ] -> [ ]
| x :: xs ->
If x >= p then x :: keepLarger p xs
Else keepLarger p xs;;

Let rec filter p xs =
Match xs with 
| [ ] -> [ ]
| x :: xs ->
If p x 
then x :: filter p xs
Else filter p xs;; 

Let rec squareAll xs =
Match xs with
| [ ] -> [ ]
| x :: xs -> x * x :: squareAll xs;;

Let rec map f xs = 
Match xs with
| [ ] -> [ ]
| x :: xs -> f x :: map f xs;;

Let rec sum xs =
Match xs wit
| [ ] -> 0
| x :: xs -> x + sum xs;;

Let rec prod xs =
Match xs wit
| [ ] -> 1
| x :: xs -> x * prod xs;;

Let rec foldRight v f xs =
Match xs with
| [ ] -> v
| x :: xs -> f x (foldRight v f xs);;

Foo 0 ( + ) [1; 2; 3; 4];; for sum
Foo 1 ( * ) [1; 2; 3; 4];; for prod

Let doit xs = 
Let xs1 = filter odd xs in
Let xs2 = map square xs1 in
Foo 0 ( + ) xs2;;

Let sumT xs =
Let rec helper n xs = 
Match xs with
| [ ] -> n
| x :: xs -> helper (n + x) xs
In helper 0 xs

Let prodT xs =
Let rec helper n xs = 
Match xs with
| [ ] -> n
| x :: xs -> helper (n * x) xs
In helper 1 xs


Let rec foldLeft v f xs =
Match xs with
| [ ] -> v
| x :: xs -> foldLeft (f v x) f xs

Let length xs =
Let f _ n = n + 1
In foldRight 0 f xs
—
foldLeft 0 (fun n _ -> n + 1) xs

Let x  = fun y -> y + y
In x 3
(fun y -> y + y) 3
((+) 3) 3
6

Filter (fun x -> x > 1) (1 :: 2 :: [ ])
Match (1 :: 2 :: [ ]) with
| [ ] -> [ ]
| x :: xs -> if (fun x -> x > 1) x
Then x :: (fun x -> x > 1) x xs
Else (fun x -> x > 1) x

Opam show qcheck
#require “qcheck”;;
Open QCheck;;

Let isSorted xs =
Let rec aux x xs =
Match xs with
| [ ] -> true
| y :: xs -> x <= y && aux y xs
In match xs with
| [ ] -> true
| x :: xs -> aux x xs

Let p xs = isSorted (quicksort xs)

Let t = Test.make (list int) p;;
Test.check_exn t;;

Let prev xs = rev xs = xs
Let x = Test.make (list int) prev;;
Test.check_exn t;;
