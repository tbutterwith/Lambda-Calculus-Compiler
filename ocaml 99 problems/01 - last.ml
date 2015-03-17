let rec last = function 
 | [] -> None
 | [x] -> Some x
 | x::l -> last l
;;
