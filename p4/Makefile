
CAMLC = ocamlc
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
CAMLDEP = ocamldep

OBJS  = conjunto.cmo tipos.cmo fresca.cmo pretty.cmo tipado.cmo 
OBJS += lambda.cmo lexer.cmo parser.cmo lector.cmo main.cmo

all: lambda

lambda : $(OBJS)
	$(CAMLC) -o $@ $(OBJS)

%.ml %.mli : %.mly
	$(CAMLYACC) $<

%.ml : %.mll
	$(CAMLLEX) $<

%.cmi : %.mli
	$(CAMLC) -c $<

%.cmo : %.ml
	$(CAMLC) -c $<

depend :  $(OBJS:.cmo=.ml)
	$(CAMLDEP) $(OBJS:.cmo=.ml) *.mli > .depend

clean :
	rm -f lambda *.cmi *.cmo parser.ml parser.mli lexer.ml *% *~ 

# dependencias

ifeq (.depend,$(wildcard .depend))
include .depend
endif
