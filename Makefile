include Makefile.common

INCLUDES=-I $(LLVMOCAMLLIB) -I parsing -I typing -I codegen -I dyn
OCAMLFLAGS=-g $(INCLUDES)
OCAMLOPTFLAGS=-warn-error P -g $(INCLUDES)
OCAMLLLVMFLAGS=-cclib -lstdc++ -cclib -L. -cclib -lruntime \
    llvm.cmxa llvm_executionengine.cmxa llvm_target.cmxa llvm_scalar_opts.cmxa \
    llvm_analysis.cmxa llvm_bitwriter.cmxa llvm_bitreader.cmxa
CFLAGS=-I runtime

SUBDIRS=parsing typing runtime codegen dyn .

PARSING=parsing/ast.cmx parsing/parser.cmi parsing/parser.cmx parsing/lexer.cmx parsing/parse.cmx

TYPING=typing/types.cmx typing/typing.cmx typing/gamma.cmx typing/subst.cmx \
	typing/operations.cmx typing/unify.cmx typing/texpr.cmx typing/inferBasic.cmx 

CODEGEN=codegen/cgil.cmx codegen/lambda_lifting.cmx codegen/currying.cmx \
	codegen/monomorphize.cmx codegen/semant.cmx codegen/codegen.cmx \
	codegen/llvm_codegen.cmx

DYNAMIC=dyn/lambda.cmx dyn/value.cmx dyn/initialBasis.cmx dyn/runtime.cmx dyn/eval.cmx \
	dyn/dynpipeline.cmx dyn/main.cmx

TL_OBJS=utils.cmx $(PARSING) elaborate.cmx $(TYPING) $(CODEGEN) \
	pipeline.cmx main.cmx

DTL_OBJS=utils.cmx $(PARSING) $(DYNAMIC)

RUNTIME=libruntime$(DLLEXT)

all: tensorlang dyntl

tensorlang: $(TL_OBJS) initial_basis.bc $(RUNTIME)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLLLVMFLAGS) $(filter %.cmx, $(TL_OBJS))

dyntl: $(DTL_OBJS) 
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(filter %.cmx, $(DTL_OBJS))

$(RUNTIME) : runtime/runtime.o
	gcc $(CFLAGS) $< `$(LLVMCONFIG) --ldflags --libs` -shared -o $@

scratch: test
	./test > scratch.ll 2>&1 
	llc scratch.ll
	gcc scratch.s -o scratch

clean:
	rm -f tensorlang dyntl
	rm -f parsing/parser.ml parsing/parser.mli parsing/lexer.ml parsing/lexer.mli parsing/parser.output
	for dir in $(SUBDIRS); do \
		rm -f $$dir/*.cm[iox] $$dir/*.o *~; \
		rm -f $$dir/*.so $$dir/*.tar.*; \
		rm -f $$dir/*.s \
		rm -f scratch* \
		rm -f a.out \
		rm -f $$dir/*.ll $$dir/*.bc; \
	done

tar: clean
	tar -czf $(OUTPUT).tar.gz *
# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend
#include .depend