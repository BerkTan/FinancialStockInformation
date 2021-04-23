MODULES=author ProjectDriver driverHelpers records finder ProjectDriver_test
MAIN=ProjectDriver.byte
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=ProjectDriver_test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=ounit2,yojson,cohttp-lwt-unix,cohttp-async,lwt

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS) -tag thread $(MAIN) && ./$(MAIN) -runner sequential

test:
	$(OCAMLBUILD) -tag 'debug' -tag thread $(TEST) && ./$(TEST) -runner sequential

docs: docs-public

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLS)

clean:
	ocamlbuild -clean
	rm -rf search.zip doc.public
