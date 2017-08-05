main:
	ocamlbuild game.byte
	ocamlbuild ai.byte
	ocamlbuild main.byte

play:
	ocamlbuild game.byte
	ocamlbuild ai.byte
	ocamlbuild -pkg graphics -pkg str main.byte
	ocamlbuild -pkg graphics -pkg str runner.byte && ./runner.byte

clean:
	ocamlbuild -clean
