let rec compress = function
| x::(y::l as t) -> if x=y then compress t else x::compress t
| compressed -> compressed ;;