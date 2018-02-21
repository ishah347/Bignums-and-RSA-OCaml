all: ps3 ps3_tests

ps3: ps3.ml
	ocamlbuild -use-ocamlfind ps3.byte	

ps3_tests: ps3_tests.ml
	ocamlbuild -use-ocamlfind ps3_tests.byte		

clean:
	rm -rf _build *.byte