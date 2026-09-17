(* Déclaration des types *)

type 'a t = {
	nb : int ; (* nombre d'états : numérotés 0, 1, ..., nb-1 *)
	sigma : 'a array ;
	i : int list ;
	f : int list ;
	delta : (int * ('a option), int list) Hashtbl.t
}

(* ajoute la transition (q1,lettre,q2) à l'automate *)
let ajouter_transition (a : 'a t) (q1 : int) (lettre : 'a) (q2 : int) : unit =
	assert((q1 >= 0) && (q1 < a.nb));
	assert((q2 >= 0) && (q2 < a.nb));
	assert(Array.mem lettre (a.sigma));
	match Hashtbl.find_opt (a.delta) (q1,Some lettre) with
	| None -> Hashtbl.add (a.delta) (q1,Some lettre) [q2]
	| Some l -> if not (List.mem q2 l) then
		    Hashtbl.replace (a.delta) (q1,Some lettre) (q2::l)

(* ajoute la transition (q1,epsilon,q2) à l'automate *)
let ajouter_transition_immediate (a : 'a t) (q1 : int) (q2 : int) : unit =
	assert((q1 >= 0) && (q1 < a.nb));
	assert((q2 >= 0) && (q2 < a.nb));
	match Hashtbl.find_opt (a.delta) (q1, None) with
	| None -> Hashtbl.add (a.delta) (q1, None) [q2]
	| Some l -> if not (List.mem q2 l) then
		    Hashtbl.replace (a.delta) (q1, None) (q2::l)

(* fonctions d'affichage *)
let affiche_transitions (a : 'a t) (print_lettre : 'a -> unit) : unit =
	for i = 0 to a.nb-1 do
		match Hashtbl.find_opt a.delta (i,None) with
		| None -> ()
		| Some l -> print_int i ;
					print_string " -> " ;
					List.iter (fun x -> print_int x ; print_string " ") l ;
					print_newline ();

		for j = 0 to (Array.length a.sigma)-1 do
			match Hashtbl.find_opt a.delta (i,Some a.sigma.(j)) with
			| None -> ()
			| Some l -> print_int i ;
				    print_string "," ;
				    print_lettre a.sigma.(j) ;
				    print_string " -> " ;
				    List.iter (fun x -> print_int x ; print_string " ") l ;
				    print_newline ()
		done
	done

let affiche_automate (a : 'a t) (print_lettre : 'a -> unit) : unit =
	print_string "I : ";
	List.iter (fun x -> print_int x ; print_string " ") a.i ;
	print_newline ();
	print_string "F : ";
	List.iter (fun x -> print_int x ; print_string " ") a.f ;
	print_newline ();
	affiche_transitions a print_lettre


(* utilisez celle-ci pour afficher un automate dont l'alphabet comporte des char *)
let affiche_automate_char (a : 'a t) : unit = affiche_automate a print_char


let vers_graphe (a : 'a t) : Graphe.graphe =
let {nb=n;sigma=s;delta=d} = a in
Array.init n (fun i->
	List.flatten (
		List.map (fun a-> let Some(x)=a in x)
		(List.filter (fun l-> match l with |None -> false |_->true)
			(List.init 
				(Array.length s) 
				(fun j-> Hashtbl.find_opt d (i,Some(s.(j))))
			)
		)
	)
)

let accessible (a : 'a t) : bool array = [||]  (* TODO *)

let coaccessible (a : 'a t) : bool array = [||]  (* TODO *)

let utiles (a : 'a t) : bool array = [||]  (* TODO *)

let emonder (a : 'a t) : unit = () (* TODO *)
