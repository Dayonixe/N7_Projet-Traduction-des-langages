(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast
open Type
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string



(* Passe AstPlacement.programme -> string *)

let rec analyser_code_instruction i  =
  match i with
    | AstPlacement.Declaration(info, e) ->
      begin
        match info_ast_to_info info with
          | InfoVar(_, t, dep, reg) ->
              Tam.push (getTaille t)
              ^ analyser_code_expression e
              ^ Tam.store (getTaille t) dep reg
          | _ -> failwith "Erreur interne"
      end
    | AstPlacement.Affectation(info, e) ->
      begin
        match info_ast_to_info info with
          | InfoVar(_, t, dep, reg) ->
              analyser_code_expression e
              ^ Tam.store (getTaille t) dep reg
          | _ -> failwith "Erreur interne"
      end
    | AstPlacement.AffichageInt e ->
        analyser_code_expression e
        ^ Tam.subr "IOut"
    | AstPlacement.AffichageRat e ->
        analyser_code_expression e
        ^ Tam.call "ST" "ROut"
    | AstPlacement.AffichageBool e ->
        analyser_code_expression e
        ^ Tam.subr "BOut"
    | AstPlacement.Conditionnelle(e, b1, b2) ->
        let lsinon = getEtiquette() in
        let lfin = getEtiquette() in
        analyser_code_expression e
        ^ Tam.jumpif 0 lsinon
        ^ analyser_code_bloc b1
        ^ Tam.jump lfin
        ^ lsinon ^ "\n"
        ^ analyser_code_bloc b2
        ^ lfin ^ "\n"
    | AstPlacement.TantQue(e, b) ->
        let ldebut = getEtiquette() in
        let lfin = getEtiquette() in
        ldebut ^ "\n"
        ^ analyser_code_expression e
        ^ Tam.jumpif 0 lfin
        ^ analyser_code_bloc b
        ^ Tam.jump ldebut
        ^ lfin ^ "\n"
    | AstPlacement.Retour(e, ia) ->
      begin
        match info_ast_to_info ia with
          | InfoFun(_, retype, paramtypes) ->
            let tailleRet = getTaille retype in
            let tailleParams = List.fold_right (fun t acc -> acc + getTaille t) paramtypes 0 in
            analyser_code_expression e
            ^ Tam.return tailleRet tailleParams
          | _ -> failwith "Erreur interne"
      end
    | AstPlacement.Empty -> ""


and analyser_code_bloc (li, taillevarlocales) =
  (* concaténer le code des instructions li et "POP taillevarlocales" *)
  String.concat "" (List.map analyser_code_instruction li)
  ^ (Tam.pop 0 taillevarlocales)


and analyser_code_expression e =
  match e with
    | AstType.AppelFonction(ia, lexpr) ->
      begin
        match info_ast_to_info ia with
          | InfoFun(name, _, _) ->
            String.concat "" (List.map analyser_code_expression lexpr)
            ^ Tam.call "ST" name
          | _ -> failwith "Erreur interne"
      end
    | AstType.Ident(info) ->
      begin
        match info_ast_to_info info with
          | InfoConst(_, v) -> Tam.loadl_int v
          | InfoVar(_, t, dep, reg) -> Tam.load (getTaille t) dep reg
          | _ -> failwith "Erreur interne"
      end
    | AstType.Booleen(b) -> if b then (Tam.loadl_int 1) else (Tam.loadl_int 0)
    | AstType.Entier(i) -> Tam.loadl_int i
    | AstType.Unaire(op, e) ->
      begin
        analyser_code_expression e
        ^ match op with
          | AstType.Numerateur -> Tam.pop 0 1
          | AstType.Denominateur -> Tam.pop 1 1
      end
    | AstType.Binaire(op, e1, e2) ->
      begin
        analyser_code_expression e1
        ^ analyser_code_expression e2
        ^ match op with
          | AstType.PlusInt -> Tam.subr "IAdd"
          | AstType.PlusRat -> Tam.call "ST" "RAdd"
          | AstType.MultInt -> Tam.subr "IMul"
          | AstType.MultRat -> Tam.call "ST" "RMul"
          | AstType.EquInt -> Tam.subr "IEq"
          | AstType.EquBool -> Tam.subr "IEq"
          | AstType.Inf -> Tam.subr "ILss"
          | AstType.Fraction -> ""
      end



let analyser_code_fonction (AstPlacement.Fonction(info, _, (li, _))) =
  match info_ast_to_info info with
    | InfoFun(name, _, _) ->
      name ^ "\n"
      ^ String.concat "" (List.map analyser_code_instruction li)
      ^ Tam.halt
    | _ -> failwith "Erreur interne"



let analyser (AstPlacement.Programme(fonctions, bloc)) =
  getEntete()
  ^ String.concat "" (List.map analyser_code_fonction fonctions)
  ^ "main\n"
  ^ analyser_code_bloc bloc
  ^ Tam.halt