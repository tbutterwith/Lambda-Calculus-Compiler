### Lambda Calculus Compiler User Guide
--
####Installation
**Requirements**
In order to compile the lambda calculus compiler you must have OCaml version 4.01.0 or higher installed. The recommended install method can be found in the documentation on the OCaml website (https://ocaml.org/docs/install.html)

**Compiling**
To compile navigate to the source code directory and run the command:

``ocamlbuild -tag thread -use-ocamlfind -pkg core lambda_compiler.native
``
#### Running
Running the compiler is as simple as using the command where the file name is your lambda calculus:

``./lambda_compiler.native *file_name*.txt``

####Syntax
The compiler uses regular lambda calculus notation replacing λ with \ . Lambda expressions are expressed as follows :
`(\x.x x)(\y.y);`
which will be simplified down to: `(\y.y)`
Lambda functions should be enclosed within parenthesis with the end of an expression being marked with a semicolon.

**Key Words**
The compiler contains a number of prewritten functions accessed using key words

| Key Word   |     Function      |
|------------|-------------------|
|successor   | `λnfx.f(nfx)`|
|addition    |`λmnfx.mf(nfx)`|
| + | see addition|

#### Output
On completion the compiler will automatically print your output to the command line window you have open. Should you wish to the compiler to output to a file use the command:
``./lambda_compiler.native *file_name*.txt > *output_file_name*.txt``