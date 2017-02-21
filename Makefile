all: fadecider tools generators

tools: transform subtouniv collapse toopennwa

generators: randomnpa randomnpvpa

fadecider:
	ocamlbuild fadecider.native
	mv fadecider.native bin/fadecider

transform:
	ocamlbuild transform.native
	mv transform.native bin/transform

subtouniv:
	ocamlbuild subtouniv.native
	mv subtouniv.native bin/subtouniv

collapse:
	ocamlbuild collapse.native
	mv collapse.native bin/collapse

toopennwa:
	ocamlbuild toopennwa.native
	mv toopennwa.native bin/toopennwa

randomnpa:
	ocamlbuild randomnpa.native
	mv randomnpa.native bin/randomnpa

randomnpvpa:
	ocamlbuild randomnpvpa.native
	mv randomnpvpa.native bin/randomnpvpa

clean:
	ocamlbuild -clean
