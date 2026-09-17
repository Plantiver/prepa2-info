
type graphe = int list array;;

let parcours (g:graphe) l =
let rec aux visited g = function
| [] -> visited
| h::t -> if List.mem h visited then aux visited g t
	else aux (h::visited) g (g.(h)@t)
in let tab = aux l g l
in Array.init (Array.length g) (fun i -> List.mem i tab);;

let transpose g =
let n = Array.length g in
Array.init n (fun i -> 
	List.filter (fun a-> a>=0) (List.init n (fun j-> if List.mem i g.(j) then j else -1))
);;





