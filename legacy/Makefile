OBJS = ast.cmo errors.cmo parser.cmo scanner.cmo semantic.cmo compile.cmo seam.cmo
TESTS = \
hello

# Choose one
YACC = ocamlyacc
# YACC = menhir --explain

TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.ml errors.ml semantic.ml seam.ml \
	$(TESTS:%=tests/test-%.mc) \
	$(TESTS:%=tests/test-%.out)

# seamc should already exist
seamc : seam preprocessor
	chmod +x seamc

seam : $(OBJS)
	ocamlc -o seam $(OBJS)

preprocessor : preprocessor.ml
	ocamlc -o preprocessor $<


.PHONY : test
test : seam testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

seam.tar.gz : $(TARFILES)
	cd .. && tar czf seam/seam.tar.gz $(TARFILES:%=seam/%)

.PHONY : clean
clean :
	rm -f seam parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff *.o gen.c preprocessor

# Generated by ocamldep *.ml *.mli
ast.cmo :
ast.cmx :
compile.cmo : ast.cmo
compile.cmx : ast.cmx
errors.cmo : ast.cmo
errors.cmx : ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
preprocessor.cmo :
preprocessor.cmx :
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
seam.cmo : semantic.cmo scanner.cmo parser.cmi compile.cmo
seam.cmx : semantic.cmx scanner.cmx parser.cmx compile.cmx
semantic.cmo : errors.cmo ast.cmo
semantic.cmx : errors.cmx ast.cmx
parser.cmi : ast.cmo
