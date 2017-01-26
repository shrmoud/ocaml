(* Q1 *)
let rec factorial (x:int): int =
match x with
| 0 -> 1
| _ -> x * factorial (x-1)
;;


(* Q2 *)
(*
let rec selectionSort = function
[]->[]
| head::tail -> 
   let rec select_r small output = function 
   [] -> small::selectionSort output
   | x::xs when x < small -> select_r x(small::output) xs
   | x::xs -> select_r small (x::output) xs
    in 
   select_r first [] tail
;;

*)

(* Q3 *)
   (* let forever (x ??) : ?? = forever forever;; 
  
 
   *)

(* Q4 *)
let rec add3toList (xs: int list): int list =
   match xs with
   | [] -> []
   | head::tail -> (head+3):: (add3toList tail)
   ;;



(* Q5 *)
let rec fibo (x:int): int =
   match x with
   | 0 -> 1
   | 1 -> 1
   | _ -> fibo(x-2) + fibo(x-1)
   ;;


(* Q6 *)
let rec countOddList (xs: int list): int = 
   match xs with
   | [] -> 0
   | head::tail -> if head mod 2 = 0 then 0 + countOddList(tail) else 1 + countOddList(tail)
   ;;


(* Q7 *)
(* Write a function that takes an integer list and return sum of all elements of the list. 
 * If the list is empty then return None.*)
let rec sum (xs: int list): int =
   match xs with
   | [] -> 0
   | head::tail -> head + (sum tail)
   ;;

let sumOfList (xs: int list): int option =
match xs with
| [] -> None
| _ -> Some(sum xs)
;;

(* Q8 *)
(* Write a function that takes a list of int*string. 
 * The function produce another list of string*int using the elements of the given list. *)

let rec makeList (xs: (int * string) list): (string * int) list = 
   match xs with
   | [] -> []
   | (x,y) :: tail -> (y,x):: (makeList tail) 
   ;;
