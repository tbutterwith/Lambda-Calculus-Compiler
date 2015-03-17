let flatten list = 
let rec aux acc = function
| [] -> acc
| One x::l -> aux (x::acc) l
| Many x::l -> aux (aux acc x) l in
List.rev (aux [] list);;