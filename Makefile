MODULES=board command state main dictionary
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.ml)
MAIN=main.byte
TEST=test.byte
OCAMLBUILD = ocamlbuild -use-ocamlfind

default: build
	utop

check:
	bash checkenv.sh && bash checktypes.sh

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	export OCAMLRUNPARAM='l=5555555555'
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) 

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip
