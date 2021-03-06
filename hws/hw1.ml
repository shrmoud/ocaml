
let volumeCylinder (height:float) (radius:float) : float =
  radius *. radius *. height *. 3.14159265359 ;;

let differenceFloatNumber (first:float) (second:float) : float =
  first -. second;;

let differenceIntNumber (first:int) (second:int) : int =
  first - second;;

let checkOddNumber (number:int): bool =
  number mod 2 = 0;;


type student = {name:string; id:int; score:float};;

let stringToStudent (tuple:string * int * float) : student =
  let (sname,sid,sscore) = tuple in
  {name = sname; id = sid; score = sscore};;

let studentToString{name=sname;id = sid; score= sscore}: (string * int * float) = (sname, sid, sscore);;


let circleArea(radius:float): float =
  radius *. radius *. 3.14159265359;;
