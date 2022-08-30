(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open AstTds
open AstType

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

(* Passe AstSyntaxe.programme -> AstTds.programme *)

(* analyse_type_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre i : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_type_expression e =
  match e with
    | AstTds.Ident(info) ->
      begin
        match info_ast_to_info info with
          | InfoVar(_, t, _, _) -> (AstType.Ident info, t)
          | InfoConst(_, _) -> (AstType.Ident info, Int)
          | _ -> failwith("Cas impossible")
      end
    | AstTds.AppelFonction(info, le) ->
      begin
        match info_ast_to_info info with
          | InfoFun(_, typret, typeparams) ->
            let rec verifier_param sle stypeparams =
              match sle, stypeparams with
                | [], [] -> []
                | [], _ -> raise (TypesParametresInattendus(typeparams, []))
                | _, [] -> raise (TypesParametresInattendus(typeparams, []))
                | t1::q1, t2::q2 ->
                  let (ne, te) = analyse_type_expression t1 in
                  if te != t2 then raise (TypesParametresInattendus (typeparams, te::[]))
                  else ne::(verifier_param q1 q2) in
            (* Vérifier les typer *)
            let nle = verifier_param le typeparams in
            (AstType.AppelFonction(info, nle), typret)
          | _ -> failwith("Cas impossible")
      end
    | AstTds.Unaire(op, e) ->
      let (ne, te) = analyse_type_expression e in
      if te != Rat then
        raise (TypeInattendu (te, Rat))
      else
        begin
          match op with
            | Numerateur -> (AstType.Unaire(Numerateur, ne), Int)
            | Denominateur -> (AstType.Unaire(Denominateur, ne), Int)
        end
    | AstTds.Binaire(op, e1, e2) ->
      let (ne1, te1) = analyse_type_expression e1 in
      let (ne2, te2) = analyse_type_expression e2 in
      begin
        match op, te1, te2 with
          | Fraction, Int, Int -> (AstType.Binaire(Fraction, ne1, ne2), Rat)
          | Plus, Int, Int -> (AstType.Binaire(PlusInt, ne1, ne2), Int)
          | Plus, Rat, Rat -> (AstType.Binaire(PlusRat, ne1, ne2), Rat)
          | Mult, Int, Int -> (AstType.Binaire(MultInt, ne1, ne2), Int)
          | Mult, Rat, Rat -> (AstType.Binaire(MultRat, ne1, ne2), Rat)
          | Equ, Int, Int -> (AstType.Binaire(EquInt, ne1, ne2), Bool)
          | Equ, Bool, Bool -> (AstType.Binaire(EquBool, ne1, ne2), Bool)
          | Inf, Int, Int -> (AstType.Binaire(Inf, ne1, ne2), Bool)
          | _ -> raise (TypeBinaireInattendu (op, te1, te2))
      end
    | AstTds.Booleen(b) -> (AstType.Booleen b, Bool)
    | AstTds.Entier(v) -> (AstType.Entier v, Int)


let rec analyse_type_instruction i =
  match i with
    | AstTds.Declaration(t, i, e) ->
      let (ne, te) = analyse_type_expression e in
      if te != t then
        raise (TypeInattendu (te, t))
      else
        modifier_type_info te i;
        AstType.Declaration(i, ne)
    | AstTds.Affectation(i, e) ->
      let (ne, te) = analyse_type_expression e in
        begin
          match info_ast_to_info i with
            | InfoVar(_, t, _, _) ->
              if te != t then
                raise (TypeInattendu (te, t))
              else
                AstType.Affectation(i, ne)
            | _ -> failwith("Cas impossible")
        end
    | AstTds.Conditionnelle(e, b1, b2) ->
      let (ne, te) = analyse_type_expression e in
      if te != Bool then
        raise (TypeInattendu (te, Bool))
      else
        let nb1 = analyse_type_bloc b1 in
        let nb2 = analyse_type_bloc b2 in
        AstType.Conditionnelle(ne, nb1, nb2)
    | AstTds.TantQue(e, b) ->
      let (ne, te) = analyse_type_expression e in
      if te != Bool then
        raise (TypeInattendu (te, Bool))
      else
        let nb = analyse_type_bloc b in
        AstType.TantQue(ne, nb)
	| AstTds.Affichage (e) ->
		let (ne,te) = analyse_type_expression e in
			begin
				match te with
				| Int -> AstType.AffichageInt(ne)
				| Rat -> AstType.AffichageRat(ne)
				| Bool -> AstType.AffichageBool(ne)
				| _ -> raise (TypeInattendu(te,te))
			end
    | AstTds.Retour(e, i) ->
      let (ne, te) = analyse_type_expression e in
      begin
      match info_ast_to_info i with
        | InfoFun(_, t, _) -> 
          if te != t then
            raise (TypeInattendu (te, t))
          else
            AstType.Retour(ne, i)
        | _ -> failwith("Cas impossible")
      end
    | AstTds.Empty -> AstType.Empty


(* analyse_type_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_type_bloc li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map analyse_type_instruction li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)

(*

type fonction = Fonction of typ * string * (typ * string) list * bloc
type fonction = Fonction of typ * Tds.info_ast * (typ * Tds.info_ast ) list * bloc

*)

(* type fonction = Fonction of typ * Tds.info_ast * (typ * Tds.info_ast ) list * bloc *)
(* AstTds.Fonction (t, ia, nlp, analyse_type_bloc tdsfonction (Some ia) li) *)

(* retour *)

(* type fonction = Fonction of Tds.info_ast * Tds.info_ast list * bloc *)

let analyse_type_fonction (AstTds.Fonction(t,info,lp,li)) =
  let (nt, nlp) = List.fold_right (fun (typ, info) (acc1, acc2) -> modifier_type_info typ info; (typ::acc1, info::acc2)) lp ([], []) in
  modifier_type_fonction_info t nt info;
  AstType.Fonction(info, nlp, analyse_type_bloc li)

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  Programme (nf,nb)
