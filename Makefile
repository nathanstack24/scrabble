MODULES=board command state main dictionary
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.ml)
TEST=test.byte
OCAMLBUILD = ocamlbuild -use-ocamlfind


default: build
	utop

check:
	bash checkenv.sh && bash checktypes.sh

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) 

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip
