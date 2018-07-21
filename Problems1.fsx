//Lester Hernandez Alfonso - ID#4017986
//COP4555 - Homework#3


(*
Problem 1 (2 points) Write an F# function pair(xs,ys) that pairs the elements in two lists
into a new list:
> pair ['a';'b';'c'] [1;2;3];;
val it : (char * int) list = [('a', 1); ('b', 2); ('c', 3)]
*)

//Pair the heads of both lists. Do the same on the tails. Concatenate results.
let rec pair(xs,ys) = 
  match (xs,ys) with
  | ([], []) -> []
  | ([], ys) -> failwith "Uneven Lists"
  | (xs, []) -> failwith "Uneven Lists"
  | (x::xs, y::ys) -> (x,y)::pair(xs, ys);;

pair (['a';'b';'c'],[1;2;3]);;
pair (["one";"two";"three";"four";"five"],[1;2;3;4;5]);;
pair (['a';'b'],[1;2;3]);;



(*
Problem 2 (2 points) Write a curried F# function cartesian xs ys that takes as input two
lists xs and ys and returns a list of pairs that represents the Cartesian product of xs and ys.
(The pairs in the Cartesian product may appear in any order.) For example,
> cartesian ['a';'b';'c'] [1;2;3];;
val it : (char * int) list =
 [('a', 1); ('a', 2); ('a', 3); ('b', 1); ('b', 2); ('b', 3); ('c', 1);
 ('c', 2); ('c', 3)]
*)

//Use map to pair the head of list xs with each item in list y. Do the same for each item in the tail
//of list xs. Concatenate all results.

let rec cartesian xs ys = 
  match (xs,ys) with
  | ([], []) -> []
  | (xs, []) -> []
  | ([], ys) -> []
  | (x::xs, ys) -> (List.map (fun n -> (x,n)) ys) @ (cartesian xs ys);;

cartesian ['a';'b';'c'] [1;2;3];;



(*
Problem 3 (2 points) An F# list can be thought of as representing a set, where the order of the
elements in the list is irrelevant. Write an F# function powerset such that powerset
set returns the set of all subsets of set. For example,
> powerset [1;2;3];;
val it : int list list =
 [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]]
 *)

//Helper function that applies a function to each item of a list and concatenates each result.
//Concatenation requires that argument f is a function that returns a list.

let rec collect f = function
  | [] -> []
  | x::xs -> f x @ collect f xs;;

//Add one item at a time to each subset (starting with subset []), and keep both the resulting set,
//and each subset by using helper 'collect'. Repeat until all subsets are exhausted.

let rec powerset xs = 
  match xs with
  | [] -> [[]]
  | x::xs -> collect (fun n -> [n; x::n])(powerset xs);;

powerset [1;2;3];;
powerset [1;2;3;4];;
powerset [1;2;3;4;5];;



(*
Problem 4 (2 points) Write an F# program split(x) that splits list x into two lists y and z
such that y contains the even numbered elements of x and z contains the odd numbered element
of x. For example,
> split [1;2;3;4;5;6;7];;
val it : int list * int list = ([1; 3; 5; 7], [2; 4; 6])
*)

//I will assume that the split is done on the value of the element, and not on the index being 
//odd or even. It is not very clear on the instructions so I will assume the even valued elements 
//will go to one list, the odd valued elements will go to the other.
//Just do a simple filter for odd and even, and create a tuple with both resulting lists.

let split xs = (List.filter (fun n -> n % 2 = 1) xs, List.filter (fun n -> n % 2 = 0) xs);;

split [1;2;3;4;5;6;7];;



(*
Problem 5 (2 points + 1 bonus point) The transpose of a matrix M is the matrix obtained by
reflecting M about its diagonal. For example, the transpose of
1 4
2 5
3 6     
 is
1 2 3
4 5 6

An m-by-n matrix can be represented in F# as a list of mrows, each of which is a list of length n.
For example, the first matrix above is represented as the list [[1;4]; [2;5]; [3;6]]. Write an efficient
F# function to compute the transpose of an m-by-n matrix:
> transpose [[1;4];[2;5];[3;6]];;
val it : int list list = [[1; 2; 3]; [4; 5; 6]]
*)

//Get the head from each list in the list of lists, combine all heads together using map.
//Repeat on the tail of each list in the list of lists.
//Pattern match for a list of lists((x::xs)::ys). Use symbol _ for base case or anything else.

let rec transpose ls = 
  match ls with
  | (x::xs)::ys -> List.map List.head ls :: transpose (List.map List.tail ls)
  | _ -> [];;

transpose [[1;4];[2;5];[3;6]];;
transpose [[1;4;7];[2;5;8];[3;6;9]];;
