(*
 Secondo Progetto in OCAML
 Interprete di un dizionario

Ziccolella Francesco Giuseppe
Matricola: 588922

*)
type ide = string;;

(*tipi di dato*)

type exp =
	| Eint of int
	| Estring of string
	| Ebool of bool
	| Den of ide
	| Prod of exp * exp
	| Sum of exp * exp
	| Diff of exp * exp
	| Eq of exp * exp
	| Minus of exp
	| IsZero of exp
	| Or of exp * exp
	| And of exp * exp
	| Not of exp
	| Ifthenelse of exp * exp * exp (*guarda,if_body,else_body*)
	| Let of ide * exp * exp
	| Fun of ide * exp
	| FunCall of exp * exp
	| Letrec of ide * ide * exp * exp
	| Dict of (ide * exp) list  (*Definisco il dizionario come una lista di coppie ide , value*)
	| Filt_ of ide list  (*definisco un filtro come una lista di ide*)
	| Check of exp * ide * exp
	| Insert of exp * ide * exp
	| Update of exp * ide * exp
	| Remove of exp * ide
  | Has_key of exp * ide
	| Clear of exp
	| Iterate of exp * exp
	| Filter of exp * (ide list)
	| Fold of exp * exp;;

(*ambiente polimorfo*)
type 't env = (string * 't) list;; (*contiene valori*)

let emptyenv (x : 't) = [("",x)];;

let rec applyenv ((r : 't env), (i : ide)) = match r with
	| [(_,e)] -> e
	| (i1,e1)::xs -> if i=i1 then e1 else applyenv (xs,i)
	| [] -> failwith ("not present in this amb");;

let bind (r : 't env) (i : ide) (v : 't) = (i,v)::r;;

(*tipi esprimibili*)
type evT =
	| Int of int
	| String of string
	| Bool of bool
	| Unbound
	| FunVal of evFun
	| DictVal of (ide * evT) list
	| RecFunVal of ide * evFun
and evFun = ide * exp * evT env;;

let typecheck (s : string) (v : evT) : bool = match s with
	| "int" -> (match v with
					| Int(_) -> true
					| _ -> false)
	| "string" -> (match v with
					| String(_) -> true
					| _ -> false)
	| "bool" -> (match v with
					|Bool(_) -> true
					| _ -> false)
	| _ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
           (Int(n),Int(u)) -> Int(n*u)
         | _-> failwith ("Type error"))
  else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	  then (match (x,y) with
	           (Int(n),Int(u)) -> Int(n+u)
	         | _-> failwith ("Type error"))
	  else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
			(Int(n),Int(u)) -> Int(n-u))
  else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
			(Int(n),Int(u)) -> Bool(n=u))
	else failwith("Type error");;

let minus x = if (typecheck "int" x)
	then (match x with
	   		Int(n) -> Int(-n))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
			Int(n) -> Bool(n=0))
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
			(Bool(b),Bool(e)) -> (Bool(b||e)))
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
			(Bool(b),Bool(e)) -> Bool(b&&e))
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
			| Bool(true) -> Bool(false)
			| Bool(false) -> Bool(true))
	else failwith("Type error");;

(*Funzioni di supporto per la gestione dei dizionari*)

let rec check dict label value =
	match dict with
		| [] -> false
		| (x,y)::xs -> if (label = x && y=value) then true else check xs label value;;

let rec member lista label =
	match lista with
		| [] -> false
		| (x,y)::xs -> if (label = x) then true else member xs label;;

let rec member_idelist lista label =  (*Questa funzione la uso per la lista filter*)
match lista with
	| [] -> false
	| x::xs -> if (label = x) then true else member_idelist xs label;;

let rec removeDup dict =
	match dict with
		| [] -> []
		| (x,y)::xs -> if (member xs x) then removeDup xs else (x,y)::removeDup xs;;

let rec updatelab lista label nval =
	match lista with
		| [] -> []
		| (x,y)::xs -> if (label = x) then (x,nval)::updatelab xs label nval else (x,y)::updatelab xs label nval;;

(*Interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	| Eint n -> Int n
	| Estring s -> String s
	| Ebool b -> Bool b
	| IsZero a -> iszero (eval a r)
	| Den i -> applyenv (r,i)
	| Eq(a, b) -> eq (eval a r) (eval b r)
	| Prod(a, b) -> prod (eval a r) (eval b r)
	| Sum(a, b) -> sum (eval a r) (eval b r)
	| Diff(a, b) -> diff (eval a r) (eval b r)
	| Minus a -> minus (eval a r)
	| And(a, b) -> et (eval a r) (eval b r)
	| Or(a, b) -> vel (eval a r) (eval b r)
	| Not a -> non (eval a r)
	| Ifthenelse(a, b, c) ->
		let g = (eval a r) in
			if (typecheck "bool" g)
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard")
	| Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r))
	| Fun(i, a) -> FunVal(i, a, r) (*il valore della funzione, cioè un'espressione non ulteriormente valutabile,  è una chiusura*)
	| FunCall(f, eArg) -> (*Gestione con scoping statico*)
		let fClosure = (eval f r) in
			(match fClosure with
				| FunVal(arg, fBody, fDecEnv) ->
					eval fBody (bind fDecEnv arg (eval eArg r))
				| RecFunVal(g, (arg, fBody, fDecEnv)) ->
					let aVal = (eval eArg r) in (*Valuto il parametro attuale nell'ambiente*)
						let rEnv = (bind fDecEnv g fClosure) in (*Estendo l'ambiente con il binding tra la funzione (g) e la sua chiusura ricorsiva*)
							let aEnv = (bind rEnv arg aVal) in (*Creo un nuovo ambiente che contiene la chiusura ricorsiva e il binding tra il parametro e l'ambiente dove è valutato il parametro attuale*)
								eval fBody aEnv (*valuto il body del let nell'ultimo ambiente calcolato*)
				| _ -> failwith("non functional value"))
	| Letrec(f, i, fBody, letBody) ->
        		let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                								eval letBody r1
                								(*valuto il body del let nell'ambiente dove prima ho fatto il binding ,tra la funzione  e la sua chiusura RICORSIVA*)


	| Dict(dict) -> let thisdict = removeDup dict
										in DictVal(evalDict thisdict r)

	| Check(thisdict,lab,nv) ->    (*CONTROLLA SE C'E' UN ELEMENTO CON IDE E VAL, la uso per controllare le operazioni*)
		(match (eval thisdict r) with
			| DictVal(mydict) ->
			if (check mydict lab (eval nv r)) then failwith("GOTCHA") else eval (Ebool(false)) r
			| _ -> failwith("wrong value"))

	| Insert(thisdict,lab,valx) ->   (*Inserisce l'elemento,dà errore se esiste già un elemento con un'ide uguale *)
		(match (eval thisdict r) with
			| DictVal(mydict) -> if (member mydict lab) then failwith("Esiste già un'elemento nel dizionario con questa key") else DictVal((lab,(eval valx r))::mydict)
			| _ -> failwith("wrong value"))

	| Update(thisdict,lab,newval) -> (*Aggiorna il valore di un elemento ,dà errore se non esiste elemento con un'ide scelto *)
		(match (eval thisdict r) with
			| DictVal(mydict) -> if (member mydict lab) then DictVal(updatelab mydict lab (eval newval r)) else failwith("Elemento non presente")
			| _ -> failwith("wrong value"))

	| Remove(thisdict,lab) ->   (*Elimina un elemento ,dà errore se non esiste elemento con un'ide scelto *)
		(match (eval thisdict r) with
			| DictVal(mydict) -> if (member mydict lab) then DictVal(removeElem mydict lab) else failwith("Elemento non presente")
			| _ -> failwith("wrong value"))

	| Has_key(thisdict,lab) ->  (*Cerca un elemento*)
			(match (eval thisdict r) with
			| DictVal(mydict) -> if (member mydict lab) then failwith("Elemento presente") else failwith("Elemento non presente")
			| _ -> failwith("wrong value"))

	| Clear(thisdict) ->     (*>Svuota un Dizionario*)
		(match (eval thisdict r) with
			| DictVal(mydict) -> DictVal([])
			| _ -> failwith("wrong value"))

	| Iterate(funx,thisdict) ->   (*Applica una funzione ad ogni elemento di un Dizionario*)
		(match (eval thisdict r) with
			| DictVal(mydict) -> DictVal(applyFun mydict funx r)
			| _ -> failwith("wrong value"))

	| Fold(funx,thisdict) ->    (*Applica una funzione sequenzialmente e aggiorna i valori delle chiavi*)
			(match (eval thisdict r) with
			| DictVal(mydict) -> DictVal(fold_v mydict funx r (Int 0))
			| _ -> failwith("wrong value"))

  | Filter(dict1,fil) -> (*Filtra il dizionario attraverso una lista di ide "filt"*)
	(match (eval dict1 r) with
		| DictVal(mydict) -> DictVal(filter mydict fil)
		| _ -> failwith("wrong value"))

	and evalDict (dict : (ide * exp) list) (r : evT env) =
				(match dict with
					| [] -> []
					| (x,y)::xs -> (x, eval y r) :: evalDict xs r)
	and removeElem (dict : (ide * evT) list) (el : ide) =
				(match dict with
					| [] -> []
					| (x,y)::xs -> if (el = x) then (removeElem xs el) else (x,y)::(removeElem xs el))

  and filter (dict : (ide * evT) list) (filt : ide list) =
				match dict with
				 []->[]
        |(x,y)::xs -> if member_idelist filt x then (x,y)::(filter xs filt)
				              else filter xs filt

	and applyFun (dict : (ide * evT) list) (funx : exp) (r : evT env) =
				(match dict with
					| [] -> []
					| (x,y)::xs ->  (x, (dictFunCall funx y r) )::(applyFun xs funx r))

	and fold_v (dict : (ide * evT) list) (funx : exp) (r : evT env) (acc : evT)=
       	(match dict with
		      | [] -> []
	       	| (x,y)::xs -> (x,sum (dictFunCall funx y r) acc)::(fold_v xs funx r (sum (dictFunCall funx y r) acc)))



	and dictFunCall (funx : exp) (y : evT) (r : evT env) =
				let fClosure = (eval funx r) in
					(match fClosure with
						| FunVal(arg, fBody, fDecEnv) ->
							eval fBody (bind fDecEnv arg y)
						| RecFunVal(g, (arg, fBody, fDecEnv)) ->
								let rEnv = (bind fDecEnv g fClosure) in (*Estendo l'ambiente con il binding tra la funzione (g) e la sua chiusura ricorsiva*)
									let aEnv = (bind rEnv arg y) in (*Creo un nuovo ambiente che contiene la chiusura ricorsiva e il binding tra il parametro e l'ambiente dove è valutato il parametro attuale*)
										eval fBody aEnv (*valuto il body del let nell'ultimo ambiente calcolato*)
						| _ -> failwith("non functional value"));;

						(* ============================= * Test Inteprete Basilare * ============================= *)

            (* Definisco Ambiente*)
             let env0 = emptyenv Unbound;;

             let e1 = FunCall(Fun("y", Sum(Den "y", Eint 1)), Eint 3);;

             eval e1 env0;;

             let e2 = FunCall(Let("x", Eint 2, Fun("y", Sum(Den "y", Den "x"))), Eint 3);;

             eval e2 env0;;

						 (*Dichiaro una funzione ricorsiva che mi calcola il fattoriale*)
             let e3 = Letrec("fact","n",Ifthenelse(Eq(Den("n"),Eint(0)),Eint(1),Prod(Den("n"),FunCall(Den("fact"),Diff(Den("n"),Eint(1))))),FunCall(Den("fact"),Eint(3)));;

            eval e3 env0;;

						(* ============================= * Test Dizionario * ============================= *)

						(*Test Dizionario Vuoto*)
						let empty_magazzino = Dict([]);;

						(*Test Magazzino della traccia progetto*)

						let magazzino = Dict([("mele", Eint 430);("banane", Eint 312);("arance", Eint 525);("pere", Eint 217)]);;

            eval magazzino env0;;

						(* ============================= * Test Insert * ============================= *)

						(*Test Insert*)

						let test_add = Insert(magazzino,"kiwi", Eint 300);;

						eval test_add env0;;

						(*Exception , quando aggiungo un elemento con chiave già esistente*)

						let test_add2 = Insert(test_add,"kiwi", Eint 300);;

						(*eval test_add2 env0;;*)

						(*Execption : Errore di Tipo , creo un dizionario con elementi di diverso tipo*)

						let magazzino_except = Dict([("kiwi", Estring "finiti");("fragole", Eint 78)]);;

            (*eval magazzino_except env0;;*)

            let type_err = Iterate(Fun("x", Sum(Den "x", Eint 1)),magazzino_except);;

						(*Execption : Errore di tipo

						eval type_err env0;;*)
						(* ============================= * Test Delete / Has_Key / Clear * ============================= *)

						let test_del = Remove(magazzino,"mele");;

						(*eval test_del env0;;*)

						(*Exception , quando elimino un elemento con chiave non esistente*)

						let test_del2 = Remove(magazzino,"kiwi");;

						(*eval test_del2 env0;;*)

						(*L'elemento è nel dizionario , mi darà "Elemento presente"*)
						let test_member = Has_key(magazzino,"mele");;

						(*eval test_member env0;;*)

            (*L'elemento è stato da poco eliminato , mi darà "Elemento non presente"*)
						let test_member2 = Has_key(magazzino,"kiwi");;

						(*eval test_member2 env0;;*)

						let test_clear = Clear(magazzino);;

						(*eval test_clear env0;;*)

						(*L'elemento non è nel dizionario , mi darà "Elemento non presente"*)
						let test_member3 = Has_key(magazzino,"mele");;

						(*eval test_member env0;;*)


						(* ============================= * Test Filter * ============================= *)

						let magazzino2 = Dict([("mele", Eint 430);("banane", Eint 312);("arance", Eint 525);("pere", Eint 217)]);;

            eval magazzino2 env0;;

						let filte = Filter(magazzino2,[("mele");("banane")]);;

						(*eval filte env0;;*)

						(*L'elemento c'è quindi deve darmi "Elemento Presente"*)
						let test_member4 = Has_key(filte,"mele");;

						(*eval test_member4 env0;;*)

						(*L'elemento non c'è quindi deve darmi "Elemento non Presente"*)
						let test_member5 = Has_key(filte,"pere");;

						(*eval test_member5 env0;;*)


						(* ============================= * Test Iterate * ============================= *)

						let magazzino3 = Dict([("mele", Eint 430);("banane", Eint 312);("arance", Eint 525);("pere", Eint 217)]);;

						eval magazzino3 env0;;

						let maga3 = Iterate(Fun("x", Sum(Den "x", Eint 1)),magazzino3);;

						eval maga3 env0;;

						(*Sull'elemento è stata applicata f che incrementa di 1 le unità quindi cercando mele , 431 ottengo "GOTCHA"*)
						let test_check = Check(maga3,"mele",Eint 431);;

						(*eval test_check env0;;*)

            (* ============================= * Test Fold * ============================= *)

						(*HO INTERPRETATO LA FOLD COME UNA FUNZIONE CHE SOMMA TUTTE LE UNITA' DELLE KEY PRECENDENTI E SOSTITUISCE
						  LE UNITA' DEL KEY AL PASSO I CON LA SOMMA DELLE I-1 KEY*)


						let magazzino4 = Dict([("mele", Eint 430);("banane", Eint 312);("arance", Eint 525);("pere", Eint 217)]);;

						eval magazzino4 env0;;

						let maga4 = Fold(Fun("x", Sum(Den "x", Eint 1)),magazzino3);;

						eval maga4 env0;;

						(*Quindi se scelgo le arance le unità della key "arance" saranno 431 + 313 + 526 = 1270 , controllo e ottengo "GOTCHA"*)
						let test_check2 = Check(maga4,"arance",Eint 1270);;

						(*eval test_check2 env0;;*)

						(*Quindi se scelgo le arance le unità della key "pere " saranno 431 + 313 + 526 + 218 = 1488 , controllo e ottengo "GOTCHA"*)
						let test_check3 = Check(maga4,"pere",Eint 1488);;

						(*eval test_check3 env0;;*)
