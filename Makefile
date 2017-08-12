all:
	
	ocamlc -c utils.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -o lam utils.cmo parser.cmo lexer.cmo 
clean:
	rm lexer.ml *.cm* *.mli parser.ml
