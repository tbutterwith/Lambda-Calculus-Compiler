let rev list = 
	let rec aux new_list = function
	| [] -> new_list
	| a::t -> aux (a::new_list) t in
	aux [] list ;;