(* Expression régulière *)
type expr =
  | Vide
  | Char of char
  | Concat of expr * expr
  | Union of expr * expr
  | Etoile of expr

let epsilon = () (* TODO *)

(* Affiche une expression régulière avec une syntaxe concise *)
let rec afficher (e : expr) =
  match e with
  | Vide -> Printf.printf "Ø"
  | Char c -> Printf.printf "%c" c
  | Concat (e1, e2) -> afficher e1 ; afficher e2
  | Union (e1, e2) ->
    Printf.printf "(";
    afficher e1;
    Printf.printf "|" ;
    afficher e2;
    Printf.printf ")"
  | Etoile e1 -> 
    Printf.printf "(";
    afficher e1;
    Printf.printf ")*"

type indexed_char = char * int

(* Expression régulière linéarisée *)
type lin_expr =
  | LVide
  | LChar of indexed_char
  | LConcat of lin_expr * lin_expr
  | LUnion of lin_expr * lin_expr
  | LEtoile of lin_expr

(* Affiche une expression rationnelle linéarisé avec une syntaxe concise *)
let rec lin_afficher (e : lin_expr) =
  match e with
  | LVide -> Printf.printf "Ø"
  | LChar (c,i) -> Printf.printf "%c%d" c i
  | LConcat (e1, e2) ->
    lin_afficher e1;
    lin_afficher e2
  | LUnion (e1, e2) ->
    Printf.printf "(";
    lin_afficher e1;
    Printf.printf "|" ;
    lin_afficher e2;
    Printf.printf ")"
  | LEtoile e1 ->
    Printf.printf "(";
    lin_afficher e1;
    Printf.printf ")*"

let rec compte_char (e : expr) : int = -1 (* TODO *)

let linearise (e : expr) : lin_expr = LVide (* TODO *)

let rec reconnait_epsilon (e : lin_expr) : bool = false (* TODO*)

let rec premiers (e : lin_expr) : indexed_char list = [] (* TODO *)

let rec derniers (e : lin_expr) : indexed_char list = [] (* TODO *)

let rec facteurs (e : lin_expr) : (indexed_char*indexed_char) list = [] (* TODO *)

(* Implémentation d'un automate non-déterministe dont les états sont numerotés de 0 
   à n et l'alphabet est l'ensemble des nombres de 0 à 255 (inclus).
   Si la case transition.(i).(j), qui est une liste, contient un nombre k >= 0
   cela signifie qu'il existe une transition de i à k d'étiquette j,
   sinon qu'il n'y a pas de transition depuis i avec une étiquette j.
   Le champ acceptant indique pour chaque état s'il est acceptant.
   L'état initial est unique et toujours 0. *)
type automate = {
  transition : int list array array;
  acceptant : bool array
}

let nb_char_entier (n : int) : int =
  1 + int_of_float (log10 (float_of_int n))

let largeurs_colonnes (a : automate) =
  let n = Array.length a.transition in (* Nombre d'états *)
  let largeur : int array = Array.make 256 0 in
  let rec largeur_case (l : int list) (acc : int) : int =
    match l with
    | [] -> acc
    | t :: q -> largeur_case q (1 + (nb_char_entier t) + acc)
  in
  for c = 0 to 255 do
    for e = 0 to (n - 1) do
      largeur.(c) <- max largeur.(c) (largeur_case a.transition.(e).(c) 0)
    done
  done;
  largeur

let rec string_of_list (l : int list) : string =
  match l with
  | [] -> ""
  | [k] -> (string_of_int k)
  | t :: q -> (string_of_int t) ^ "," ^ (string_of_list q) 

(* Affiche la matrice des transitions de l'automate *)
let afficher_matrice_automate (a : automate) =
  let n = Array.length a.transition in (* Nombre d'états *)
  (* Taille de chaque colonne *)
  let largeurs = largeurs_colonnes a in
  (* En-tête *)
  let cmax = nb_char_entier n in
  Printf.printf "  %*s " cmax "";
  for i = 0 to 255 do
    if largeurs.(i) <> 0
    then Printf.printf "| %*s%c " largeurs.(i) "" (char_of_int i)
  done;
  Printf.printf "|\n";
  (* Pour chaque état de départ d'une transition *)
  for s = 0 to n - 1 do
    (* Une astérisque indique que l'état est acceptant *)
    if a.acceptant.(s)
    then Printf.printf "*"
    else Printf.printf " ";
    Printf.printf " %*d " cmax s;
    (* Pour chaque lettre de l'alphabet *)
    for i = 0 to 255 do
      if largeurs.(i) <> 0
      then begin
        if a.transition.(s).(i) = []
        then Printf.printf "| %*s " largeurs.(i) ""
        else Printf.printf "| %*s " largeurs.(i) (string_of_list a.transition.(s).(i))
      end
    done;
    Printf.printf "|\n"
  done

exception No_transition

let accepte (a : automate) (m : string) : bool = false (* TODO *)


let construire (e : expr) = (* TODO*)
  let n = 10 in (* À modifier*)
  let transition = Array.make_matrix n 256 [] in
  let acceptant = Array.make n false in
  {
    transition = transition;
    acceptant = acceptant
  }