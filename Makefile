MODULES=board state main command author move gui constants
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
GUI=gui.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -package camlimages.all 

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

gui: 
	$(OCAMLBUILD) -tag 'debug' $(GUI) && OCAMLRUNPARAM=b ./$(GUI) 

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private final.zip

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

loc: 
	cloc --by-file --include-lang=OCaml .

zip:
	zip dama.zip *.ml* *.md *.sh *.jpg *.jpeg _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	images
