(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open AstType
open AstPlacement
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme



(* AstType.instruction -> int -> string -> (AstPlacement.instruction * int) *)
let rec analyser_placement_instruction i dep reg =
	match i with
	| AstType.Declaration (info,e) ->
		begin
			match info_ast_to_info info with
			| InfoVar (_,t,_,_) -> modifier_adresse_info dep reg info;
				(AstPlacement.Declaration (info,e), getTaille t)
			| _ -> failwith "erreur interne"
		end
	| AstType.Affectation (info,e) -> (AstPlacement.Affectation (info,e), 0)
	| AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
	| AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
	| AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
	| AstType.Conditionnelle (e,b1,b2) ->
		let nb1 = analyser_placement_bloc b1 dep reg in
			let nb2 = analyser_placement_bloc b2 dep reg in
				(AstPlacement.Conditionnelle (e,nb1,nb2), 0)
	| AstType.TantQue (e,b) ->
		let nb = analyser_placement_bloc b dep reg in
				(AstPlacement.TantQue (e,nb), 0)
    | AstType.Retour (e,ia) -> (* avec ia = info de la fonction où apparait ce return*)
		begin
			match info_ast_to_info ia with
			| InfoFun (_,_,_) -> (AstPlacement.Retour (e,ia), 0)
			| _ -> failwith "erreur interne"
		end
    | AstType.Empty -> (AstPlacement.Empty, 0)



(* AstType.bloc -> int -> string -> (AstPlacement.bloc * int) *)
and analyser_placement_bloc li dep reg =
	(* analyser successivement les instructions en faisant avancer
			le déplacement selon la taille demandée par l'instruction *)
	(* renvoyer le nouveau bloc et la taille qu'il occupe *)
	match li with
	| [] -> ([], 0)
	| i::q ->
		let (ni,ti) = analyser_placement_instruction i dep reg in
			let (nq,tq) = analyser_placement_bloc q (dep+ti) reg in
				(ni::nq,ti+tq)


let analyser_placement_fonction (AstType.Fonction(info, lp, b)) =
  let rec placer_parametres params offset =
    match params with
      | [] -> ()
      | p::q ->
        begin
          match info_ast_to_info p with
            | InfoVar(_, t, _, _) ->
              modifier_adresse_info (offset - getTaille t) "LB" p;
              placer_parametres q (offset - getTaille t)
            | _ -> failwith "Erreur interne"
        end
  in
  placer_parametres (List.rev lp) 0;
  let nb = analyser_placement_bloc b 3 "LB" in
  (AstPlacement.Fonction(info, lp, nb))


let analyser (AstType.Programme(fonctions, bloc)) =
  let nf = List.map analyser_placement_fonction fonctions in
  let nb = analyser_placement_bloc bloc 0 "SB" in
  AstPlacement.Programme(nf, nb)
