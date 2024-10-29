ML = ocamllex
YACC_ML = ocamlyacc
OCAMLC = ocamlc

# Cibles de test
test: ast.cmo types.cmo test.ml test_typage.ml
	$(OCAMLC) -o test ast.cmo test.ml
	$(OCAMLC) -o test_typage ast.cmo types.cmo test_typage.ml

# Génération des fichiers ast
ast.cmo: ast.ml  
	$(OCAMLC) -c ast.ml  

# Génération des fichiers types
types.cmo: ast.cmo types.ml  
	$(OCAMLC) -c types.ml  

# Nettoyage
clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm -f *~
