ML = ocamllex
YACC_ML = ocamlyacc
OCAMLC = ocamlc

# Cibles de test
test: ast.cmo types.cmo parser.cmo lexer.cmo test.ml test_typage.ml
	$(OCAMLC) -o test ast.cmo test.ml
	$(OCAMLC) -o test_typage ast.cmo types.cmo test_typage.ml
	$(OCAMLC) -o test_integration ast.cmo types.cmo parser.cmo lexer.cmo test_integration.ml

# Génération des fichiers lexer et parser
lexer.ml: lexer.mll
	$(ML) lexer.mll

parser.ml parser.mli: parser.mly
	$(YACC_ML) parser.mly

# Compilation des fichiers
ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

types.cmo: ast.cmo types.ml
	$(OCAMLC) -c types.ml

parser.cmo: parser.ml parser.mli
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c parser.ml

lexer.cmo: lexer.ml
	$(OCAMLC) -c lexer.ml

test_integration.cmo: test_integration.ml
	$(OCAMLC) -c test_integration.ml

# Nettoyage
clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli test test_typage test_integration
