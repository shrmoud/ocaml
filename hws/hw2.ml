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
let rec sumOfList (xs: int list): int options =
match xs with
| [] -> None
| head::tail -> head + (sumOfList tail)
;;

(* Q7 *)
let tup = (int * string)
