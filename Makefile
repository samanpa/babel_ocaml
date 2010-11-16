OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
DLLEXT=.so
CPP=gcc

include Makefile.generated


INCLUDES=-I $(LLVMOCAMLLIB)
OCAMLFLAGS=-g $(INCLUDES)
OCAMLOPTFLAGS=-warn-error P -g $(INCLUDES)
OCAMLLLVMFLAGS=-cclib -lstdc++ -cclib -L. -cclib -lruntime \
    llvm.cmxa llvm_executionengine.cmxa llvm_target.cmxa llvm_scalar_opts.cmxa \
    llvm_analysis.cmxa llvm_bitwriter.cmxa llvm_bitreader.cmxa

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .ll .bc
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
#enabling this causes us to try and compile the llvm libraries
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
.mll.ml:
	$(OCAMLLEX) $<
.mly.ml:
	$(OCAMLYACC) -v $<
.cc.o:
	$(CPP) -I. `$(LLVMCONFIG) --cxxflags` $< -c -o $@
.cc.ll:
	clang -O3 -S -emit-llvm -I . $< -o $@
.ll.bc:
	llvm-as $< -o $@

OUTPUT=tensorlang

# The list of object files for the language
OUTPUT_OBJS=utils.cmx \
	ast.cmx elaborate.cmx parser.cmx lexer.cmx parse.cmx \
	types.cmx typing.cmx texpr.cmx \
	gamma.cmx subst.cmx operations.cmx unify.cmx inferBasic.cmx \
	cgil.cmx lambda_lifting.cmx currying.cmx semant.cmx \
	codegen.cmx llvm_codegen.cmx \
	pipeline.cmx main.cmx

RUNTIME=libruntime$(DLLEXT)

all: INITIAL_BASIS $(OUTPUT) 

INITIAL_BASIS : initial_basis.bc

$(OUTPUT): $(OUTPUT_OBJS) $(RUNTIME)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLLLVMFLAGS) $(OUTPUT_OBJS)

$(RUNTIME) : runtime.o
	gcc $< `$(LLVMCONFIG) --ldflags --libs` -shared -o $@

scratch: test
	./test > scratch.ll 2>&1 
	llc scratch.ll
	gcc scratch.s -o scratch
test: test.cmx
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLLLVMFLAGS) $<

parser.cmx: parser.ml
	$(OCAMLOPT) -c parser.mli
	$(OCAMLOPT) -c $<

# Clean up
clean:
	rm -f $(OUTPUT) test
	rm -f *.cm[iox] *.o *~ 
	rm -f *.so *.tar.*
	rm -f *.s
	rm -f parser.ml parser.mli lexer.ml lexer.mli parser.output
	rm -f scratch*
	rm -f a.out
	rm -f *.ll *.bc

tar: clean
	tar -czf $(OUTPUT).tar.gz *
# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend
#include .depend

