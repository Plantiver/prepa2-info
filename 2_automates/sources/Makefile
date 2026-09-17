all:
	ocamlc -g graphe.ml automate.ml automateAsynchrone.ml automateDeterministe.ml tests.ml -o a.out
	echo "run" | ocamldebug a.out

clean:
	rm *.out *.cmi *.cmo
