Let x = e1
In e2

(Let x = 5 in x + x);; - int = 10
(Let x = 5 in x) + x;; - unbound value x

Let x = 5 in
Let y = 10 in
X + y;; - Int =15
X;; - unbound value x
Y;; - unbound value y

Let definition - let x = v;;
Let expression - let x2 = v2 in body;;

Let y = 10 in
(let z = 30 in z) + y;; - int = 40

Let y = 10 in
(let z = 30 in z) + z;; - unbound value z

Shadowing
Let x = 4 in
Let x = 8 in 
X;; - int = 8

Let x = 20 in
Let x = 10 + x in (second x is bound to 20)
X;; - int = 50

Let x = 20 in
(let x  = 10 + x in (second x is bound to 20)
x) + x;; - int 50 (first x is bound to 10 and second is bound to 20)

(6, 10);; - int * int = (6, 10)
((3, 4), true);; - (int * int) * bool = ((3, 4), true)

Let f x = (x, x+1);;
F 100;; - int * int = (100, 101)

Let add3 t =
Let (x,y,z) = t in x+ y +z;;
Add3 (1,2,3);; - int = 6

Let x = 100 in
(let x = 5 in x) + x (second x bound to the first x, third x bound to the x in line above)
Int = 105
—
Let x = 100 in 
(let y = 5 in x) + x (second+third x are bound to x in line above and y is unused)

Let x = 100 in 
(let x = 5 in x) + x (second x bound to the first x, third x bound to the x in line above)
Int = 105
—
Let x = 100 in
(let y = 5 in y) + x (second y bound to first y, x bound to x in line above)
Int = 105

Let x = 100 in
(let y = 5 in y) + x
Int = 105

Let x = 100 in
(let x = 5 in x) + x
Int = 105

Let x = 100 in 
(let y = 5 in x) + x
Int = 200, y is unused

Let y = 1 in
Let x = 2 in
Let x = 3 in
(x,y)
int*int = (3,1)

Let max3 x y z =
(x,y,z);;

Let reciprocal (x,y) = 
(y,x);;

Let isValid (x,y) =
Y <> 0;;

Let fracToFloat (x,y) = 
Float_of_int x /. Float_of_int y;;

Let rec gcd a b =
If b = 0 then a 
Else gcd b (a mod b);;
—
Let simplify (x,y) =
If y = 0 then
Failwith “Division by 0”
Else if x = 0 then
(0,1)
Else let cd = gcd x y in
(x / cd, y / cd);;

Let rec gcd a b =
If b = 0 then a 
Else gcd b (a mod b);;
—
Let add (a,b) (c,d) = 
Let x = (a * d) + (c * b) in
Let y = b * d in
Let cd = gcd x y in
(x / cd, y / cd);;

Let rec gcd a b =
If b = 0 then a 
Else gcd b (a mod b);;
—
Let mult (a,b) (c,d) =
Let x = a * c in
Let y = b * d in
Let cd = gcd x y in
(x / cd, y / cd);;

5. Let transposition ((a,b),(c,d)) =
((a,c),(b,d));;

Let matrixMul ((a,b),(c,d)) ((e,f),(g,h)) =
Let w = (a*e) + (b*g) in
Let x = (a*f) + (b*h) in
Let y = (c*e) + (d*g) in
Let z = (c*f) + (d*h) in
((w,x),(y,z));;
