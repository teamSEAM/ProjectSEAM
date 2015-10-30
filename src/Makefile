EXEC = test
INCLUDES = core,util

.PHONY : build
build:
	ocamlbuild -Is $(INCLUDES) -verbose 1 $(EXEC).native | grep -v cache
	mv $(EXEC).native $(EXEC)

.PHONY : clean
clean :
	ocamlbuild -clean
