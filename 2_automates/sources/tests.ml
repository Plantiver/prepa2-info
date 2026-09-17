
(* Exemple : automate a0 *)
let (a0 : char AutomateDeterministe.t) = {
	nb = 6;
	sigma = [|'a';'b'|];
	f = [1;4];
	delta = Hashtbl.create 1
}

let _ =
	AutomateDeterministe.ajouter_transition a0 0 'a' 1;
	AutomateDeterministe.ajouter_transition a0 0 'b' 0;
	AutomateDeterministe.ajouter_transition a0 1 'a' 3;
	AutomateDeterministe.ajouter_transition a0 1 'b' 2;
	AutomateDeterministe.ajouter_transition a0 2 'a' 5;
	AutomateDeterministe.ajouter_transition a0 2 'b' 1;
	AutomateDeterministe.ajouter_transition a0 3 'a' 4;
	AutomateDeterministe.ajouter_transition a0 3 'b' 0;
	AutomateDeterministe.ajouter_transition a0 4 'a' 0;
	AutomateDeterministe.ajouter_transition a0 4 'b' 5;
	AutomateDeterministe.ajouter_transition a0 5 'a' 5;
	AutomateDeterministe.ajouter_transition a0 5 'b' 1

let _ = 
	let accessibles = AutomateDeterministe.accessible a0 in
	for i = 0 to a0.nb - 1 do
		Printf.printf "%d est %s accessible\n" i (if accessibles.(i) then "" else "non ")
	done

(* Exemple : automate a0 *)
let (a1 : char AutomateAsynchrone.t) = {
	nb = 3;
	sigma = [|'a';'b'|];
	i = [0; 2];
	f = [0];
	delta = Hashtbl.create 1
}

let _ =
	AutomateAsynchrone.ajouter_transition a1 0 'b' 1;
	AutomateAsynchrone.ajouter_transition_immediate a1 0 2;
	AutomateAsynchrone.ajouter_transition a1 1 'a' 1;
	AutomateAsynchrone.ajouter_transition a1 1 'a' 2;
	AutomateAsynchrone.ajouter_transition a1 1 'b' 2;
	AutomateAsynchrone.ajouter_transition a1 2 'a' 1

let _ = 
	let accessibles = AutomateAsynchrone.accessible a1 in
	for i = 0 to a1.nb - 1 do
		Printf.printf "%d est %s accessible\n" i (if accessibles.(i) then "" else "non ")
	done