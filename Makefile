all:
	ocamlc -c utils.ml	
	ocamlc -c type_check.ml
	ocamlc -c gen_code.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c linker.ml
	ocamlc -c compile.ml
	ocamlc -o lam utils.cmo type_check.cmo gen_code.cmo parser.cmo lexer.cmo linker.cmo compile.cmo
clean:
	rm lexer.ml *.cm* *.mli parser.ml
