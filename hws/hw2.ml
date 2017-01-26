(* Q1 *)
let rec factorial (x:int): int =
match x with
| 0 -> 1
| _ -> x * factorial (x-1)
;;


(* Q2 *)

 (* Gets the minimum element of the list *)
 let rec min lst =
    match lst with
      x::[] -> x
      | (x::tl) -> let mintl = min tl in
      if x < mintl then x else mintl
;;


 (* Removes a given element from the list *)
let rec remove(x,lst) =
     match lst with
       [] -> []
     | (y::tl) -> if x=y then tl else y::(remove(x,tl))
;;


 (* Sorts a list by repeatedly extracting the minimum *)
let rec selectionSort(lst) =
     match lst with
       [] -> []
     | _  -> let m = min lst in m::(selectionSort(remove(m,lst)))
;;


(* Q3 *)
   (* let forever (x ??) : ?? = forever forever;; 
  
     This is a trick question. It has no type. The function never returns. So, we cannot find out the input parameters. It is a forever loop.
 
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
