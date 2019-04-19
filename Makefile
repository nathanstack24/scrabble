MODULES=board command state main dictionary
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.ml)
MAIN=main.byte
TEST=test.byte
OCAMLBUILD = ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,qcheck,ANSITerminal

default: build
	utop

check:
	bash checkenv.sh && bash checktypes.sh

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && OCAMLRUNPARAM="l=5555555555" ./$(MAIN)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-hide-warnings
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) 

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip
