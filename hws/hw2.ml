(* Q1 *)
let rec factorial (x:int): int =
   x * (fact x-1)
;;


(* Q2 *)


(* Q3 *)
   (* let forever (x ??) : ?? = forever forever;; 
   *  
   *
   *)

(* Q4 *)
let rec add3toList (xs: int list): int list =
   match xs with
   | [] -> []
   | head::tail -> (head*3):: (add3toList tail)
   ;;



(* Q5 *)
let rec fibo (x:int): int =
   match x with
   | 0 -> 1
   | 1 -> 1
   | _ -> x * (fibo x-1)
   ;;


(* Q6 *)
let rec countOddList (xs: int list): int = 
   match xs with
   | [] -> []
   | head::tail -> if head mod 2 = 0 then 1+(countOddList tail) else 0+(countOddLit tail)
   ;;


(* Q7 *)
(* Write a function that takes an integer list and return sum of all elements of the list. 
 * If the list is empty then return None.*)
let rec sumOfList (xs: int list): int options =
   match xs with
   | [] -> None
   | head::tail -> head + (sumOfList tail)
   ;;

(* Q8 *)
(* Write a function that takes a list of int*string. 
 * The function produce another list of string*int using the elements of the given list. *)

let rec makeList (xs: (int * string) list): (string * int) = 
   match xs with
   | [] -> []
   | (x,y) :: tail -> (y,x):: (makeList tail) 
   ;;
