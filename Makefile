all:
	ocamlc -c -g utils.ml	
	ocamlc -c -g type_check.ml
	ocamlc -c -g gen_code.ml
	ocamlyacc  parser.mly
	ocamlc -c -g parser.mli
	ocamllex lexer.mll
	ocamlc -c -g lexer.ml
	ocamlc -c -g parser.ml
	ocamlc -c -g linker.ml
	ocamlc -c -g compile.ml
	ocamlc -g -o lam utils.cmo type_check.cmo gen_code.cmo parser.cmo lexer.cmo linker.cmo compile.cmo
clean:
	rm lexer.ml *.cm* *.mli parser.ml
