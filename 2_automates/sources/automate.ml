(* Déclaration des types *)

type 'a mot = 'a list

type 'a t = {
  nb : int; (* nombre d'états : numérotés 0, 1, ..., nb-1 *)
  sigma : 'a array;
  i : int list;
  f : int list;
  delta : (int * 'a, int list) Hashtbl.t;
}

(* ajoute la transition (q1,lettre,q2) à l'automate *)
let ajouter_transition (a : 'a t) (q1 : int) (lettre : 'a) (q2 : int) : unit =
  assert (q1 >= 0 && q1 < a.nb);
  assert (q2 >= 0 && q2 < a.nb);
  assert (Array.mem lettre a.sigma);
  match Hashtbl.find_opt a.delta (q1, lettre) with
  | None -> Hashtbl.add a.delta (q1, lettre) [ q2 ]
  | Some l ->
      if not (List.mem q2 l) then Hashtbl.replace a.delta (q1, lettre) (q2 :: l)

(* fonctions d'affichage *)
let affiche_transitions (a : 'a t) (print_lettre : 'a -> unit) : unit =
  for i = 0 to a.nb - 1 do
    for j = 0 to Array.length a.sigma - 1 do
      match Hashtbl.find_opt a.delta (i, a.sigma.(j)) with
      | None -> ()
      | Some l ->
          print_int i;
          print_string ",";
          print_lettre a.sigma.(j);
          print_string " -> ";
          List.iter
            (fun x ->
              print_int x;
              print_string " ")
            l;
          print_newline ()
    done
  done

let affiche_automate (a : 'a t) (print_lettre : 'a -> unit) : unit =
  print_string "I : ";
  List.iter
    (fun x ->
      print_int x;
      print_string " ")
    a.i;
  print_newline ();
  print_string "F : ";
  List.iter
    (fun x ->
      print_int x;
      print_string " ")
    a.f;
  print_newline ();
  affiche_transitions a print_lettre

(* utilisez celle-ci pour afficher un automate dont l'alphabet comporte des char *)
let affiche_automate_char (a : 'a t) : unit = affiche_automate a print_char

let accessible (a : 'a t) : bool array =
  let vus = Array.make a.nb false in
  let rec visit s =
    vus.(s) <- true;
    for j = 0 to Array.length a.sigma - 1 do
      match Hashtbl.find_opt a.delta (s, a.sigma.(j)) with
      | None -> ()
      | Some l -> List.iter (fun s' -> if not vus.(s') then visit s') l
    done
  in
  List.iter (fun s -> visit s) a.i;
  vus

(*<>*)

let voisins s a =
  List.flatten
    (List.map
       (fun k ->
         let (Some x) = k in
         x)
       (List.filter
          (fun l -> match l with None -> false | _ -> true)
          (List.init (Array.length a.sigma) (fun j ->
               Hashtbl.find_opt a.delta (s, a.sigma.(j))))))

let vers_graphe (a : 'a t) : Graphe.graphe =
  Array.init a.nb (fun i -> voisins i a)

let rec can_access s t i a =
  if t = s then true
  else if i = 0 then false
  else List.exists (fun b -> can_access t b (i - 1) a) (voisins s a)

let accessible (a : 'a t) : bool array =
  Array.init a.nb (fun j -> List.exists (fun i -> can_access i j a.nb a) a.i)

let coaccessible (a : 'a t) : bool array =
  Array.init a.nb (fun j -> List.exists (fun f -> can_access j f a.nb a) a.f)

let utiles (a : 'a t) : bool array =
  Array.map2 (fun a b -> a && b) (accessible a) (coaccessible a)

let emonder (a : 'a t) : unit =
  let ut = utiles a in
  Hashtbl.filter_map_inplace
    (fun (i, _) l ->
      let res =
        List.filter_map (fun j -> if ut.(i) && ut.(j) then Some j else None) l
      in
      if List.length res > 0 then Some res else None)
    a.delta

let est_deterministe (a : 'a t) =
  if List.length a.i <> 1 then false
  else
    let est_det = ref true in
    for i = 0 to a.nb - 1 do
      for j = 0 to Array.length a.sigma - 1 do
        match Hashtbl.find_opt a.delta (i, a.sigma.(j)) with
        | None | Some [ _ ] | Some [] -> ()
        | _ -> est_det := false
      done
    done;
    !est_det

type array_set = bool array

let appartient v a = a.(v)
let vide n = Array.make n false
let est_vide a = a = vide (Array.length a)
let union a b = Array.map2 (fun a b -> a || b) a b
let intersection a b = Array.map2 (fun a b -> a && b) a b

let ajoute a b =
  let a1 = Array.sub a 0 (Array.length b) in
  let a2 = Array.sub a (Array.length b) (Array.length a) in
  Array.append (union a1 b) a2

let construit l n =
let res = vide n in
let rec modify acc = function
| [] -> acc
| h::t -> acc.(h)<-true; modify acc t in
modify res l; res











