let rec last_two = function
	| [] -> None
	| [x] -> None
	| [x;y] -> Some (x,y)
	| x::l -> last_two l
;;