all:
	ocamlc -c -g utils.ml	
	ocamlfind ocamlc -package why3  -g unix.cma -g str.cma -g dynlink.cma -g nums.cma -g why3.cma  -c -g why3_interface.ml
	ocamlc -c -g type_check.ml
	ocamlc -c -g gen_code.ml
	ocamlyacc  -v parser.mly
	ocamlc -c -g parser.mli
	ocamllex lexer.mll
	ocamlc -c -g lexer.ml
	ocamlc -c -g parser.ml
	ocamlc -c -g linker.ml
	ocamlc -c -g compile.ml
	ocamlfind ocamlc -package why3  -g unix.cma -g str.cma -g dynlink.cma -g nums.cma -g why3.cma  -g utils.cmo -g why3_interface.cmo  type_check.cmo gen_code.cmo parser.cmo lexer.cmo linker.cmo compile.cmo -o lam
clean:
	rm lexer.ml *.cm* *.mli parser.ml
