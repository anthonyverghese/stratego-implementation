default:
	ocamlbuild -use-ocamlfind main_cmd.byte && ./main_cmd.byte

gui:
	ocamlbuild -use-ocamlfind GUI.byte && ./GUI.byte

test:
	ocamlbuild -use-ocamlfind board_test.byte && ./board_test.byte

clean:
	ocamlbuild -clean
