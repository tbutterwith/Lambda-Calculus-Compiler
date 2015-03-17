let rec length = function
| [] -> 0
| [x] -> 1
| x::l -> 1 + length l
;;