(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open AstType
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme



(* AstTds.expression -> (AstType.expression, typ) *)
(*let rec analyser_type_expression e =
	match e with
	| AstTds.Ident (info) ->
		begin
			match info_ast_to_info info with
			| InfoVar (_,t,_,_) -> (AstType.Ident(info),t)
			| InfoConst (_,_) -> (AstType.Ident(info),Int)
			| _ -> failwith ("Cas impossible")
		end
	| AstTds.AppelFonction (info,le) ->
		begin
			(* regarder dans info pour obtenir InfoFun (_,typeret,typeparams) *)
			match info_ast_to_info info with
			| InfoFun (_,typeret,_) ->
				(* analyser les le pour obtenir une liste de couple(ne,te) *)
				(* vérifier que la liste des te = typeparams *)
				let lne = List.map (fun e -> fst (analyser_type_expression e)) le in
					(* renvoyer (AstType.AppelFonction(info, liste des ne), typeret) *)
					(AstType.AppelFonction(info,lne),typeret)
			| _ -> failwith ("Cas impossible")
		end
	| AstTds.Unaire (op,e) ->
		(* (ne,te) := analyser_type_expression e *)
		let (ne,te) = analyser_type_expression e in
			(* vérifier te = rat *)
			(* renvoyer (AstType.Unaire(op,ne), Int) *)
			if te = Rat then
				match op with
				| Numerateur -> (AstType.Unaire(Numerateur,ne),Int)
				| Denominateur -> (AstType.Unaire(Denominateur,ne),Int)
			else raise (TypeInattendu (te,Rat))
	| AstTds.Binaire (op,e1,e2) ->
		begin
			let (ne1,te1) = analyser_type_expression e1 in
				let (ne2,te2) = analyser_type_expression e2 in
					match op,te1,te2 with
					| Fraction,Int,Int -> (AstType.Binaire(Fraction,ne1,ne2),Rat)
					| Plus,Int,Int -> (AstType.Binaire(PlusInt,ne1,ne2),Int)
					| Plus,Rat,Rat -> (AstType.Binaire(PlusRat,ne1,ne2),Rat)
					| Mult,Int,Int -> (AstType.Binaire(MultInt,ne1,ne2),Int)
					| Mult,Rat,Rat -> (AstType.Binaire(MultRat,ne1,ne2),Rat)
					| Equ,Int,Int -> (AstType.Binaire(EquInt,ne1,ne2),Int)
					| Equ,Bool,Bool -> (AstType.Binaire(EquBool,ne1,ne2),Bool)
					| Inf,Int,Int -> (AstType.Binaire(InfInt,ne1,ne2),Int)
					| _ -> raise (TypeBinaireInattendu(op,te1,te2))
    	end
	| AstTds.Booleen (b) -> (AstType.Booleen(b),Bool)
	| AstTds.Entier (v) -> (AstType.Entier(v),Int)



(* AstTds.instruction -> AstType.instruction *)
let rec analyser_type_instruction i =
	match i with
	| AstTds.Declaration (t,i,e) ->
		let (ne,te) = analyser_type_expression e in
			(* verifier te = t *)
			if te = t then
			begin
				(* modifier_type_info i t *)
				modifier_type_info te i;
				(* renvoyer AstType.Declaration(i,ne) *)
				AstType.Declaration(i,ne)
			end
			(* sinon exception TypeInattendu *)
			else raise (TypeInattendu(te,t))
	| AstTds.Affectation (i,e) ->
		(* (ne,te) := analyser_type_expression e *)
    	let (ne,te) = analyser_type_expression e in
			let InfoVar(_,t,_,_) = info_ast_to_info i in
				(* vérifier que te = type contenu dans i *)
				(* renvoyer AstType.Affectation(i,ne) *)
				if te = t then AstType.Affectation(i,ne)
				else raise (TypeInattendu(te,te))
	| AstTds.Conditionnelle (e,b1,b2) ->
		(* (ne,te) := analyser_type_expression e *)
		let (ne,te) = analyser_type_expression e in
			(* vérifier te = bool *)
			if te = Bool then
				(* nb1 := analyser_type_bloc b1 *)
				let nb1 = analyser_type_bloc b1 in
					(* nb2 := analyser_type_bloc b2 *)
					let nb2 = analyser_type_bloc b2 in
						(* renvoyer AstType.Condionnelle(ne,nb1,nb2) *)
						AstType.Conditionnelle(ne,nb1,nb2)
			else raise (TypeInattendu(te,te))
	| AstTds.TantQue (e,b) -> failwith ("")
	| AstTds.Affichage (e) ->
		(* (ne,te) := analyser_type_expression e *)
		let (ne,te) = analyser_type_expression e in
			begin
				(* selon te, renvoyer : *)
				match te with
				(* AstType.AffichageInt(ne) ou *)
				| Int -> AstType.AffichageInt(ne)
				(* AstType.AffichageRat(ne) ou *)
				| Rat -> AstType.AffichageRat(ne)
				(* AstType.AffichageBool(ne) *)
				| Bool -> AstType.AffichageBool(ne)
				| _ -> raise (TypeInattendu(te,te))
			end
	| AstTds.Retour (e,i) ->
		(* (ne,te) := analyser_type_expression e *)
		let (ne,te) = analyser_type_expression e in
			(* verifier que te = type de retour contenu dans info_ast_to_info i *)
			(* renvoyer AstType.Retour(ne,i) *)
			if te = info_ast_to_info i then AstType.Retour(ne,i)
			else raise (TypeInattendu(te,te))
	| AstTds.Empty -> AstType.Empty



and analyser_type_bloc li = List.map (analyser_type_instruction) li*)