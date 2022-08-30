(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
  open Tds
  open Exceptions
  open Ast
  open AstTds

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme

(* Passe AstSyntaxe.programme -> AstTds.programme *)

(*
(* Expressions de Rat *)
type expression =
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AppelFonction of string * expression list
  (* Accès à un identifiant représenté par son nom *)
  | Ident of string
  (* Booléen *)
  | Booleen of bool
  (* Entier *)
  | Entier of int
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | Unaire of unaire * expression
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | Binaire of binaire * expression * expression
*)

(* = expression de AstTds *)
(* type expression =
  | AppelFonction of Tds.info_ast * expression list
  | Ident of Tds.info_ast
  | Booleen of bool
  | Entier of int
  | Unaire of unaire * expression
  | Binaire of binaire * expression * expression *)

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =
(* (AstTds.Booleen true) (* failwith "todo"*) *)
  match e with
    | AstSyntax.AppelFonction(name, liste_expressions) ->
       begin
        match chercherGlobalement tds name with
          | None ->
            (* L'identifiant n'est pas trouvé dans la tds globale. *)
            raise (IdentifiantNonDeclare name)
          | Some info ->
            (* L'identifiant est trouvé dans la tds globale,
              il a donc déjà été déclaré. L'information associée est récupérée. *)
              begin
                match info_ast_to_info info with
                | InfoFun(_, _, _) -> AstTds.AppelFonction(info, List.map (analyse_tds_expression tds) liste_expressions)
                |  _ ->
                  (* Cet identifiant ne correspond pas à une fonction *)
                  raise (MauvaiseUtilisationIdentifiant name)
              end
        end
    | AstSyntax.Ident(name) ->
      begin
      match chercherGlobalement tds name with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds globale. *)
          raise (IdentifiantNonDeclare name)
        | Some info ->
          (* L'identifiant est trouvé dans la tds globale,
            il a donc déjà été déclaré. L'information associée est récupérée. *)
            begin
              match info_ast_to_info info with
              | InfoVar(_, _, _, _) -> AstTds.Ident(info)
              | InfoConst(_, i) -> AstTds.Entier(i)
              |  _ ->
                (* Cet identifiant ne correspond pas à une variable ou constante *)
                raise (MauvaiseUtilisationIdentifiant name)
            end
        end
    | AstSyntax.Booleen(bool) -> AstTds.Booleen(bool)
    | AstSyntax.Entier(i) -> AstTds.Entier(i)
    | AstSyntax.Unaire(unaire, expression) ->
      AstTds.Unaire(unaire, analyse_tds_expression tds expression)
    | AstSyntax.Binaire(binaire, expression1, expression2) -> 
      AstTds.Binaire(binaire, (analyse_tds_expression tds expression1), (analyse_tds_expression tds expression2))


(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (n,e) ->
      begin
        match chercherGlobalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds globale. *)
          raise (IdentifiantNonDeclare n)
        | Some info ->
          (* L'identifiant est trouvé dans la tds globale,
          il a donc déjà été déclaré. L'information associée est récupérée. *)
          begin
            match info_ast_to_info info with
            | InfoVar _ ->
              (* Vérification de la bonne utilisation des identifiants dans l'expression *)
              (* et obtention de l'expression transformée *)
              let ne = analyse_tds_expression tds e in
              (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information
              et l'expression remplacée par l'expression issue de l'analyse *)
               Affectation (info, ne)
            |  _ ->
              (* Modification d'une constante ou d'une fonction *)
              raise (MauvaiseUtilisationIdentifiant n)
          end
      end
  | AstSyntax.Constante (n,v) ->
      begin
        match chercherLocalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la tds de la constante *)
          ajouter tds n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage ne
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
  begin
    (* On récupère l'information associée à la fonction à laquelle le return est associée *)
    match oia with
    (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
    | None -> raise RetourDansMain
    (* Il y a une information -> l'instruction est dans une fonction *)
    | Some ia ->
      (* Analyse de l'expression *)
      let ne = analyse_tds_expression tds e in
      Retour (ne,ia)
  end


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
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

let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li)) =
  begin
    (* On vérifie que la fonction n'est pas déjà déclarée *)
    match chercherLocalement maintds n with
    | Some _ ->
        raise (DoubleDeclaration n)
    | None ->
      (* Création de la tds de la fonction *)
      let tdsfonction = creerTDSFille maintds in
      (* Création de l'info associée à la fonction *)
      let info = InfoFun (n, t, (List.map fst lp)) in

      (* Création du pointeur sur l'information *)
      let ia = info_to_info_ast info in

      (* Ajout de l'information (pointeur) dans la tds *)
      ajouter maintds n ia;
      
      (* Conversion des paramètres en paramètres de l'AstDTS *)
      let nlp = List.map (fun (typ, name) ->
                  begin
                    (* On vérifie que ce paremètre n'est pas déjà déclaré *)
                    match chercherLocalement tdsfonction name with
                      | Some _ ->
                        raise (DoubleDeclaration name)
                      | None ->
                        let info = InfoVar (name, Undefined, 0, "") in
                        (* Création du pointeur sur l'information *)
                        let ia = info_to_info_ast info in
                        (* Ajout de l'information (pointeur) dans la tds *)
                        ajouter tdsfonction name ia;
                        (typ, ia) 
                  end) lp in
      (* Renvoie de la fonction tdsnom a été remplacé par l'information
      et les paramètres par leur information *)
      AstTds.Fonction (t, ia, nlp, analyse_tds_bloc tdsfonction (Some ia) li)
  end

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  Programme (nf,nb)
