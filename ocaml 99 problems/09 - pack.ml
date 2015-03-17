let pack list =
	let rec aux current acc = function
	| [] -> []
	| [x] -> (x::current)::acc
	| x::(y::_ as t) -> 
		if x = y then aux (x::current) acc t 
		else aux [] ((x::current)::acc) t in
	List.rev (aux [] [] list) ;;