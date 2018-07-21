//Lester Hernandez Alfonso - ID#4017986
//COP4555 - Homework#4

(*
Problem 1 (2 points) Given vectors u = (u1, u2 ,..., un) and v = (v1, v2 ,..., vn), the inner
product of u and v is defined to be u1*v1 + u2*v2 + ... + un*vn. Write a curried F#
function inner that takes two vectors represented as int lists and returns their inner product
(assuming the two lists are of equal length):
> inner [1;2;3] [4;5;6];;
val it : int = 32
*)

//A function that multiplies items at the same index in two equal length lists.
//It then aggregates the results of all multiplications.

let rec inner u v =
  match (u,v) with
  | ([],[]) -> 0
  | (x::xs, y::ys) -> x*y + inner xs ys
  | _ -> failwith "Unequal lists";;

inner [1;2;3] [4;5;6];;



//------------------------------------------------------------------------------------------------

(*
Problem 2 (2 points) Given an m-by-n matrix A and an n-by-k matrix B, the product of A and B is
an m-by-k matrix whose entry in position (i,j) is the inner product of row i of A with
column j of B. For example,

1 2 3
4 5 6
 X
0 1
3 2
1 2
 =
9  11
21 26

Write an uncurried F# function to do matrix multiplication (assuming the dimensions of two
given matrices are appropriate):
> multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]]);;
val it : int list list = [[9; 11]; [21; 26]]
Hint: Use transpose (from previous assignment), inner, and List.map.
*)

//A version of the transpose function from Homework 3.

let rec transpose ls = 
  match ls with
  | (x::xs)::ys -> List.map List.head ls :: transpose (List.map List.tail ls)
  | _ -> [];;

//Do the inner product of the first row (head) of matrix A and every item in the 
//transpose of list B. Repeat for the rest of rows in matrix A (tail). 
//Until no items are left in list A. Append all results.

let rec multiply (A,B) = 
  match (A,B) with
  | ((x::xs)::ys, (u::us)::vs) -> (List.map (inner (List.head A)) (transpose B)) :: multiply (List.tail A, B)
  | _ -> [];;

multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]]);;



//------------------------------------------------------------------------------------------------------

(*
Problem 3 (3 points) Write mergesort in F#:
A merge sort works as follows recursively:
(1) Divide the unsorted list into two sublists;
(2) Sort the two sublists;
(3) Merge two sorted sublists to produce a new sorted list.
• Use the split function in Homework #3 to accomplish step (1);
• Write a recursive function merge to merge to sorted sublists into one sorted list to do (3);
• Write a recursive function mergesort, which uses the split function to obtain two unsorted
sublists from one unsorted list, recursively calls itself on the two unsorted sublists, and then
uses merge to get the final sorted list.
*)

//A version of the split function from Assignment 3.
let split list =
   let rec build l (odd, even) =
      match l with
      | [] -> (List.rev odd, List.rev even)
      | x::xs -> match (List.length l % 2) with
                 | 0 -> build xs (odd ,x::even)
                 | _ -> build xs (x::odd, even)
   build list ([],[]);;

split [1;2;3;4;5;6;7];;


//Merge two sorted sublists u and v into a single sorted one.
//Append all items in list v less than the head x of list u to x. 
//Repeat for the next item in u and the items in v that are greater than or equal to x
//until all items in v have been exhausted. Append all results into a single list.

let rec merge u v = 
  match (u,v) with
  | ([],[]) -> []
  | (x::xs, []) -> u
  | ([], y::ys) -> v
  | (x::xs, y::ys) -> List.filter (fun n -> n < x) v @ [x] @ merge xs (List.filter (fun n -> n >= x) v);;

merge [9; 13; 19; 31] [7; 8; 14; 35];;


//Split initial list, into two sublists using split function.
//Call mergesort recursively to keep splitting resultant sublists until only atomic sublists remain.
//Use merge to sort all sublists in pairs of 2, starting with the atomic pairs (sorted by definition)
//as recursive calls return.

let rec mergesort ls = 
  match ls with
  | [] -> []
  | [x] -> [x]
  | x::y::ys -> let (first,second) = split ls in merge (mergesort (first)) (mergesort (second));;

mergesort [9; 13; 19; 31; 7; 8; 14; 35];;
mergesort [9; 19; 31; 7; 13; 8; 35; 14];;



//-----------------------------------------------------------------------------------------------------  
(*
Problem 4 (3 points) Write an F# program to evaluate arithmetic expressions written in the
language given by the following context-free grammar:
E -> n | -E | E + E | E - E | E * E | E / E | (E)
In the above, n is an integer literal, -E is the negation of E, the next four terms are the sum,
difference, product, and quotient of expressions, and (E) is used to control the order of evaluation
of expressions, as in the expression 3*(5-1).
(1) Use F# type definition (discriminated union type) to define an abstract syntax tree grammar
based on the above concrete grammar by completing the following partial solution:
type Exp = Num of int| Prod of Exp * Exp | ...
(hint: add one constructor in the discriminated union type for each rule. No constructor is needed
for the parentheses rule). The example given above would simply be represented by:
Prod(Num 3, Diff(Num 5, Num 1)).
*)

type Exp = Num of int| Neg of Exp| Sum of Exp * Exp| Diff of Exp * Exp| Prod of Exp * Exp| Quot of Exp * Exp;;

(*
(2) To deal with possible division by zero, we make use of the built-in F# type in our evaluate
function:
type 'a option = None | Some of 'a
evaluate returns Some n in the case of a successful evaluation, and None in the case of an
evaluation that fails due to dividing by zero. For example,
> evaluate (Prod(Num 3, Diff(Num 5, Num 1)));;
val it : int option = Some 12
> evaluate (Diff(Num 3, Quot(Num 5, Prod(Num 7, Num 0))));;
val it : int option = None
*)

type 'a option = None | Some of 'a;;

(*
(3) Write a recursive function evaluate to evaluate arithmetic expressions defined using the type
definitions in (1) & (2). evaluate e should use pattern matching based on the discriminated
union type to evaluate each of e's sub-expressions; it should also distinguish between the
cases of successful or failed sub-evaluations. To get you started, here is the beginning of the
definition of evaluate:

let rec evaluate = function
| Num n -> Some n
| Neg e -> match evaluate e with
           | Some n -> Some (-n)
           | None -> None
| Sum (e1, e2) -> match (evaluate e1, evaluate e2) with
                  |...
| ...
*)

let rec evaluate = function
| Num n -> Some n
| Neg e -> match evaluate e with
           | Some n -> Some (-n)
           | None -> None
| Sum (e1, e2) -> match (evaluate e1, evaluate e2) with
                  | (Some n, Some m) -> Some (n + m)
                  | _ -> None
| Diff (e1, e2) -> match (evaluate e1, evaluate e2) with
                   | (Some n, Some m) -> Some (n - m)
                   | _ -> None
| Prod (e1, e2) -> match (evaluate e1, evaluate e2) with
                   | (Some n, Some m) -> Some (n * m)
                   | _ -> None
| Quot (e1, e2) -> match (evaluate e1, evaluate e2) with
                   | (Some n, Some 0) -> None
                   | (Some n, Some m) -> Some (n / m)
                   | _ -> None;;

evaluate (Prod(Num 3, Diff(Num 5, Num 1)));;
evaluate (Diff(Num 3, Quot(Num 5, Prod(Num 7, Num 0))));;
evaluate (Diff(Num 10, Quot(Num 35, Prod(Num 7, Num 1))));;


//--------------------------------------------------------------------------------------
(*
Problem 5 (1 point – bonus) Describe two features of F# that you have difficulty to understand
and use.

A/ 
   I believe that the most difficult concepts of F# for me are related to recursion. I initially
struggled to understand how every loop and its body could be substituted by a corresponding recursive
function. I had previously been exposed to this concept in other classes, but at the time I failed to
envision how this could be done. That particular feature of F# - and programming languages in general -
is not very intuitive to most people. I was no different. It took me days to start thinking in a recursive
manner instead of iterative. It still takes a significant amount of time for me to understand complex
recursive functions, or to come up with ones such as the problems in the assignments. However, I'm getting
better the more I practice.
   The second concept I was initially confused about was polymorphic recursive types. It is one of the topics
of this assignment. Although less confusing than loops as recursion, it was still difficult to grasp how 
data structures like trees could be defined in terms of recursion over a single starting element that would
enclose every other element in the structure. Now that I understand the concept, it seems more intuitive and 
smarter than the way a tree structure is described as solely pointer based in the previous imperative languages
I have been exposed to.
*)









             

