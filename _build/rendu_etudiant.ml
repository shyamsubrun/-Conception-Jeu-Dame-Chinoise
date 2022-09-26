let dim = 3

type case = int * int * int

type couleur =
	| Vert
	| Jaune
	| Rouge
	| Noir
	| Bleu
	| Marron
	| Libre (*case libre du plateau*)
	| Dehors
	(*case en dehors du plateau, utile pour l'affichage*)
	| Nombre of int
	(*pour mettre des petits noms*)
	| Nom of string

let string_of_couleur x =
	match x with
	| Vert -> "Vert"
	| Jaune -> "Jaune"
	| Rouge -> "Rouge"
	| Noir -> "Noir"
	| Bleu -> "Bleu"
	| Marron -> "Marron"
	| Libre -> "Libre"
	| Dehors -> "Dehors"
	| Nombre n -> string_of_int n
	(*pour mettre des petits noms*)
	| Nom s -> s


type case_coloree = case * couleur

type configuration = case_coloree list * couleur list

type coup = Du of case * case | Sm of case list

let configuration_initial = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ])

let liste_joueurs (_, l) = l

let mis_a_jour_configuration _ _ = Error "To do"

let gagnant _ = Libre


(** QUESTION 2 *)

let est_dans_losange case = match case with
	(i,j,k) -> ((i+j+k=0) && (j<=dim && j>=(-dim)) && ((i<=(dim-k) && i>=(-dim-k) && k<=dim && k>=(-dim) )));;
(** EXAMPLES *)
(** est_dans_losange (3, 2, -5);; *)
(** est_dans_losange (4, 2, -6);; *)
(** est_dans_losange (3, 2, -5);; *)
(** est_dans_losange (6, -3, -3);; *)
(** est_dans_losange (3, -6, 3);; *)
(** est_dans_losange (1, -5, 4);; *)
(** est_dans_losange (2, -1, -1);; *)
(** est_dans_losange (0, 2, -2);; *)
(** est_dans_losange (-3, 3, 0);; *)
(** est_dans_losange (-4, 0, 4);; *)



(** QUESTION 3 *)

let est_dans_etoile c = match c with 
		(i,j,k)-> if ((i>=(-dim) && j>=(-dim) && k>=(-dim) && i+j+k=0)||(i<=dim && j<=dim && k<=dim && i+j+k=0)) then true else false ;; 


(********************** Fonctions auxiliaires *)

(**  Test si une cases est egale a une autre case coloree *)
let cases_egaux c1 c2 = match c1 with
  (i, j ,k) -> (match c2 with
    ((l, m, n), p) -> if(i=l && j=m && k=n) then true
               else false);;
(** Example (cases_egaux (1,2,3) ((1,2,3),Rouge));; *)

(**  Retourne la couleur d'une case coloree *)
let couleur_case c = match c with 
  ((i,j,k) , b) -> b;;

  (**  Test si une case c est dans la liste l *)
let rec case_est_dans_liste c l = match l with
  |[] -> false
  |h::t -> if(cases_egaux c h) then true
           else (case_est_dans_liste c t);;

  (**  Test si une case c est dans la config *)

  (**  QUESTION 8 *)
let case_dans_config c cc = match cc with
  (a, b) -> (case_est_dans_liste c a);;

(********************** QUESTION 4 et 9 *)

(**  Retourne la couleur d'une case coloree dans une liste  *)
(**  example quelle_couleur_dans_liste (1,2,3) [((1,2,2), Rouge), ((1,2,3),Jaune)];; -> Jaune  *)
let rec quelle_couleur_dans_liste c l= match l with
  |[] -> Libre
  |h::t -> if(cases_egaux c h) then (couleur_case h) else (quelle_couleur_dans_liste c t);;

(**  Retourne la couleur d'une case dans une config *)
let quelle_couleur c cc = if est_dans_etoile c then (**Libre*)
  (
    if(case_dans_config c cc) then 
    (
      match cc with 
        |(a,b) -> (quelle_couleur_dans_liste c a)
    )
    else Libre
  )
	else Dehors;;


(********************** QUESTION 5 *)

let tourne_case m c = match c with 
	(i,j,k)-> if(m mod 6) =1 then (-k,-i,-j)
		else if (m mod 6) = 2 then (j,k,i)
		else if ( m mod 6) = 3 then (-i,-j,-k)
		else if ( m mod 6) = 4 then (j,i,k)
		else if ( m mod 6) = 5 then (-j,-k,-i)
		else (i,j,k);;

(********************** QUESTION 6 *)
let tourne_case_coloree m c = match c with 
	(i,j)-> ((tourne_case m i), j);;

let rec tourne_cases_coloree m l = match l with 
	|[]-> []
  |h::t -> (tourne_case_coloree m h)::(tourne_cases_coloree m t)
  ;;
(** tourne_cases_coloree 1 [((1,2,3),Rouge); ((3,2,1),Marron)];; *)

let perm l= match l with
  |[] -> []
  |h::t -> t @ [h];;

let rec tourne_config c1 = match c1 with
  |(a, b) -> ((tourne_cases_coloree 1 a), (perm b));;



let sont_cases_voisines c cc = match c with 
	|(i,j,k)-> match cc with 
		|(x,y,z) -> if i=x && j= y+1 && k=z-1 && i+j+k=0 && x+y+z=0 then true 
				else if i=x && j=y-1 && k=z+1 && i+j+k=0 && x+y+z=0 then true
				else if i=x+1 && j=y && k=z-1 && i+j+k=0 && x+y+z=0 then true
				else if i=x-1 && j=y && k=z+1 && i+j+k=0 && x+y+z=0 then true
				else if i=x+1 && j=y-1 && k=z && i+j+k=0 && x+y+z=0 then true
				else if i=x-1 && j=y+1 && k=z && i+j+k=0 && x+y+z=0 then true 
				else false ;;

(** Example *)
(** sont_cases_voisines (1,0,-1) (2,2,2);; *)
(** sont_cases_voisines (1,0,-1) (2,0,-2);; *)







(********************** QUESTION 10 *)

let rec cherche_ligne_droite c l h = match h with
	|0 -> c::l
	|x -> match c with
		(i,j,k) -> (cherche_ligne_droite (i,j,k) ((i+h,j,k-h)::l) (h-1));;
(** cherche_ligne_droite (-6,3,3) [] 2;; *)

let rec cherche_ligne_opposee c l h = match h with
	|0 -> c::l
	|x -> match c with
		(i,j,k) -> (cherche_ligne_opposee (i,j,k) ((i+h,j-h,k)::l) (h-1));;
(** cherche_ligne_opposee (-6,3,3) [] 2;; *)
(** (cherche_ligne_opposee (-8,4,4) [] 3);; *)
(** cherche_ligne_opposee (-7, 4, 3) [] 2;; *)

let rec toute_cases_par_lignes l x = match x with
	|0 -> l
	|y -> match l with
			|[] -> []
			|h::t -> (cherche_ligne_opposee h [] (y-1)) @ (toute_cases_par_lignes t (y-1));;
(** toute_cases_par_lignes (cherche_ligne_droite (-8,4,4) [] 3) 4;; *)

let rec triangle_par_lignes_coloree l couleur = match l with
	|[] -> []
	|h::t -> (h,couleur) :: triangle_par_lignes_coloree t couleur;;

let remplir_triangle configuration couleur case = match configuration with
	(a,b) -> ((a @ (triangle_par_lignes_coloree (toute_cases_par_lignes (cherche_ligne_droite case [] (dim-1)) dim) couleur)), b);;

(** remplir_triangle configuration_initial Jaune (-8,4,4);; *)


(********************** QUESTION 11 *)

let rec remplir_init_rec liste_joueur config = match liste_joueur with
	|[] -> config;
	|h::t -> remplir_init_rec t (tourne_config (remplir_triangle config h (-(2*dim),dim,dim)));;

let remplir_init liste_joueur = (remplir_init_rec liste_joueur ([], liste_joueur));;


let configuration_initial = remplir_init [Vert; Jaune; Rouge; Noir; Bleu; Marron];;


(** QUESTION 12 *)
let est_dep_unit configuration case1 case2 = match configuration with
	(a,b) -> match b with
		|[] -> false
		|h::t -> (((quelle_couleur case1 configuration) = h) && (sont_cases_voisines case1 case2) && ((quelle_couleur case2 configuration) = Libre) && est_dans_losange case2);;


(** QUESTION 13 *)

let rec remplacer_case liste_cases case1 case2 = match case1 with
	(i,j,k) -> match case2 with 
		(l,m,n) -> match liste_cases with
			|[] -> liste_cases
			|(o,p)::t -> if o=case1 then 
							(case2,p)::t 
						 else
						 	(o,p)::(remplacer_case t case1 case2);;

let fait_dep_unit configuration case1 case2 = match configuration with
	(a,b) -> ((remplacer_case a case1 case2),b);;



(** QUESTION 14 *)

let mis_a_jour_configuration conf c = match c with
	|Du (x,y) -> (if (est_dep_unit conf x y) then
					(Ok (tourne_config (fait_dep_unit conf x y)))
			  else (Error "ce n’est pas un déplacement unitaire 😡 😡 😡"))
	|_ -> Error "Tu ne peux choisir que deux cases pour le moment 🤓";;

	

(** QUESTION 15 *)

let add_case case1 case2 = match case1 with
	(a,b,c) -> match case2 with	
		(d,e,f) -> (a+d,b+e,c+f);;

let diff_case case1 case2 = match case1 with
	(a,b,c) -> match case2 with	
		(d,e,f) -> (a-d,b-e,c-f);;


(** QUESTION 16 *)

(** Example calcul_pivot (2, -3, 1) (-3, 1, 2);; *)
(** Example calcul_pivot (2, -3, 1) (-2, 1, 1);; *)
(** Principe milieu de (a,b,c) et (d,e,f) = (a+d/2, b+e/2, c+f/2) *)

let calcul_pivot case1 case2 = match case1 with
	(a,b,c) -> match case2 with	
		(d,e,f) -> if a != d && b != e && c != f then None 
				else if (( (a mod 2) = 0 &&  (d mod 2) != 0) || ( (a mod 2) = 1 &&  (d mod 2) = 0)) || (( (b mod 2) = 0 &&  (e mod 2) != 0) || ( (b mod 2) = 1 &&  (e mod 2) = 0)) || (( (c mod 2) = 0 &&  (f mod 2) != 0) || ( (c mod 2) = 1 &&  (f mod 2) = 0)) then None
				else Some ((a+d)/2,(b+e)/2,(c+f)/2) ;;


(** QUESTION 17 *)
(** Example vec_et_dist (0, 2, -2) (0,0,0);; *)
(** Les case1 et case2 doivent etre sur la meme ligne pour que ca fonctionne *)
let vec_et_dist case1 case2 = match diff_case case1 case2 with
	(a,b,c) -> if a > 0 then ((a/a, b/a, c/a),a)
			else if b>0 then ((a/b, b/b, c/b),b)
			else ((a/c, b/c, c/c),c);;


(** QUESTION 18 *)
(** Exemple est_libre_seg (4, -3, -1) (-3, 4, -1) configuration_initial ;; *)
(** Exemple est_libre_seg (2, -5, 3) (2, 2, -4) configuration_initial ;; *)

let est_case_libre c cc = match quelle_couleur c cc with
	Libre -> true
	|_ -> false;;

let rec est_libre_seg case1 case2 config = match vec_et_dist case1 case2 with
	(x,d) -> if d = 1 then true
				else (est_case_libre (add_case case2 x) config) && (est_libre_seg case1 (add_case case2 x) config);;


(** QUESTION 19 *)

let est_saut_valide case1 case2 config = match (calcul_pivot case1 case2) with
			Some case3 -> if est_case_libre case3 config then false
				else (est_libre_seg case1 case3 config) && (est_libre_seg case3 case2 config)
			|_ -> false;;


let est_saut case1 case2 config = (est_saut_valide case1 case2 config)  && ((quelle_couleur case2 config) = Libre) && est_dans_losange case2;;


(** QUESTION 20 *)
let cut = function
  | [] -> [] 
  | _::xs -> xs

  
let rec est_saut_multiple l config = match l with
	[] -> true
	|h::[] -> true
	|h::i::[] -> (est_saut h i config)
	|h::i::t -> (est_saut h i config) && (est_saut_multiple (cut l) config);;



(** QUESTION 21 *)
let rec last l = match l with
    | [] -> None
    | [x] -> Some x
    | h::t -> last t;;

let est_tour_joueur case1 config = match config with
	(a,b) -> match b with
		|[] -> false
		|h::t -> ((quelle_couleur case1 config) = h);;


let mis_a_jour_configuration conf c = match c with
	|Du (x,y) -> (if (est_dep_unit conf x y) then
					(Ok (tourne_config (fait_dep_unit conf x y)))
				else (Error "ce n’est pas un déplacement unitaire 😡 😡 😡"))
	|Sm l -> (if (est_saut_multiple l conf) then 
				match l with
					h::t -> if (est_tour_joueur h conf = false) then Error "Ce n'est pas ton tour 🤓"
							else (match (last t) with
								Some lastElem -> (Ok (tourne_config (fait_dep_unit conf h lastElem)))
								|_ -> Error "Le saut multiple choisit n'est pas valide 🤓" )
					|_ -> Error "Le saut multiple choisit n'est pas valide 🤓" 
			 else Error "Le saut multiple choisit n'est pas valide 🤓" );;



(** QUESTION 22 *)
let case_arrivee_1 = (4, -3, -1);;
let case_arrivee_2 = (4, -2, -2);;
let case_arrivee_3 = (4, -1, -3);;

let direction_haut_gauche = (1,-1,0);;
let direction_haut_droite = (1,0,-1);;

let direction_opposee d = if d = direction_haut_gauche then direction_haut_droite else direction_haut_gauche;;

(**Exemple distance_case ((-4, 3, 1),Vert) direction_haut_gauche;; --> 8*)
(**Exemple distance_case ((1, 0, -1),Vert) direction_haut_gauche;; --> 3*)
let rec distance_case case_coloree direction = match case_coloree with
	(case,color) -> (if est_dans_losange (add_case case direction) then
						(if (add_case case direction) = case_arrivee_1 || (add_case case direction) = case_arrivee_2 || (add_case case direction) = case_arrivee_3 then
							1
						else 1 + distance_case ((add_case case direction), color) (direction_opposee direction))
					else distance_case case_coloree (direction_opposee direction));;


let rec dist_but_couleur l counter = if counter = 0 then 0
									 else match l with
									 	h::t -> (distance_case h direction_haut_gauche) + (dist_but_couleur t (counter-1))
										|_ -> 0;;


(** # dist_but configuration_initial;; *)
(** - : int = 52 *)

let dist_but config = (match config with
			(a,b) -> dist_but_couleur a 6);;

(** # gagne configuration_initial;; *)
(** - : bool = false *)
let gagne config = if (dist_but config) = 0 then true else false;;

(** QUESTION 23 *)

let protagoniste config = match (liste_joueurs config) with
	[] -> None
	|h::t -> Some h;;

let rec est_partie config l = match l with
	[] -> Libre
	|h::t -> match mis_a_jour_configuration config h with
				Ok c2 -> (if gagne c2 then
							(match protagoniste c2 with 
								Some x -> x 
								|None -> Dehors)
						 else est_partie c2 t)
				| Error e -> Dehors;;