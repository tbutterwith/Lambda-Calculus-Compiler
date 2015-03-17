let rec at k = function
| [] -> None
| x::l -> if k = 1 then Some x else at (k-1) l
;;