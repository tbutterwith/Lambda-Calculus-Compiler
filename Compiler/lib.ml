open Lambda_type

let succ 	= Lambda ( 'n', Lambda ('s', Lambda('z', App( Char 's', App(Char 'n', App (Char 's', Char 'z'))))))
let add		= Lambda ( 'm', Lambda ('n', Lambda ('f', Lambda ('x', App(Char 'm', App(Char 'f', App(Char 'n', App(Char 'f', Char 'x'))))))))
let mult 	= Lambda ( 'p', Lambda ('q', App( Char 'q', App(add, App( Char 'p', Lambda ('s', Lambda ('z', Char 'z')))))))

let alpha_list = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';]