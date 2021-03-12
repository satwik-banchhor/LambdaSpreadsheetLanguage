main: make clean

make:
	ocamlc -c backend.ml
	ocamllex lexer.mll	# generates lexer.ml
	ocamlyacc parser.mly	# generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o exe backend.cmo lexer.cmo parser.cmo main.cmo
	./exe <Input.txt

clean:
	@rm -f *.cmo
	@rm -f *.cmi
	@rm -f *.mli
	@rm -f lexer.ml
	@rm -f parser.ml
	@rm -f exe