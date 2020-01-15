type ide = string;;
type exp = Eint of int | Ebool of bool | EString of string | Den of ide | Prod of exp * exp | Sum of exp * exp | Diff of exp * exp |
	Eq of exp * exp | Minus of exp | IsZero of exp | Or of exp * exp | And of exp * exp | Not of exp |
	Ifthenelse of exp * exp * exp | Let of ide * exp * exp | Fun of ide * exp | FunCall of exp * exp |
	Letrec of ide * exp * exp | Dictionary of dict | Insert of ide * exp * exp | Delete of exp * ide |
	Has_Key of ide * exp | Iterate of exp * exp | Fold of exp * exp | KeyList of keyList | Filter of exp * exp
and dict = Empty | Item of ide * exp * dict
and keyList = EndList | StrItem of ide * keyList;;

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type evT = Int of int | Bool of bool | String of string | Unbound | FunVal of evFun | RecFunVal of ide * evFun | DictionaryVal of (ide * evT) list | KeyListVal of ide list
and evFun = ide * exp * evT env

(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	"int" -> (match v with
		Int(_) -> true |
		_ -> false) |
	"bool" -> (match v with
		Bool(_) -> true |
		_ -> false) |
	"string" -> (match v with
		String(_) -> true |
		_ -> false) |
	"dictionary" -> (match v with
		DictionaryVal(_) -> true |
		_ -> false) |
	"keylist" -> (match v with
		KeyListVal(_) -> true |
		_ -> false) |
	_ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u))
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
		Bool(true) -> Bool(false) |
		Bool(false) -> Bool(true))
	else failwith("Type error");;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n |
	Ebool b -> Bool b |
	EString s -> String s |
	IsZero a -> iszero (eval a r) |
	Den i -> applyenv r i |
	Eq(a, b) -> eq (eval a r) (eval b r) |
	Prod(a, b) -> prod (eval a r) (eval b r) |
	Sum(a, b) -> sum (eval a r) (eval b r) |
	Diff(a, b) -> diff (eval a r) (eval b r) |
	Minus a -> minus (eval a r) |
	And(a, b) -> et (eval a r) (eval b r) |
	Or(a, b) -> vel (eval a r) (eval b r) |
	Not a -> non (eval a r) |
	Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard") |
	Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) |
	Fun(i, a) -> FunVal(i, a, r) |
	Dictionary(d) -> DictionaryVal(evalDict [] d r) |
	KeyList(kl) -> KeyListVal(convertKeyList kl) |
	Insert(key, e1, dict) -> 
				(match eval dict r with
					DictionaryVal(dic) -> DictionaryVal(insertDic key (eval e1 r) dic) |
					_ -> failwith("Not a dictionary")) |
	Delete(dict, key) -> 
				(match eval dict r with
					DictionaryVal(dic) -> DictionaryVal(deleteFromDic key dic) |
					_ -> failwith("Not a dictionary")) |
	Has_Key(key, dict) -> 
				(match eval dict r with
					DictionaryVal(dic) -> Bool(memberDict key dic) |
					_ -> failwith("Not a dictionary")) |
	Iterate(f, d) ->
				(match d with
					Dictionary(dic) -> DictionaryVal(iterDic f dic r) |
					_ -> failwith("Not a dictionary")) |
	Fold(f, d) ->
				(match d with
					Dictionary(dic) -> sumDic f dic r |
					_ -> failwith("Not a dictionary")) |
	Filter(kl, d) -> 
				(match d,kl with
					Dictionary(dic), KeyList(kLis) -> DictionaryVal(filterDic kLis dic r) |
					_, KeyList(kList) -> failwith("Not a dictionary")	|
					Dictionary(dic), _ -> failwith("Not a list of keys") |
					_, _ -> failwith("Not a dictionary and not a list of keys")) |
	FunCall(f, eArg) -> 
		let fClosure = (eval f r) in
			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r)) |
				RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv |
				_ -> failwith("non functional value")) |
        Letrec(f, funDef, letBody) ->
        		(match funDef with
            		Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                         			                eval letBody r1 |
            		_ -> failwith("non functional def"))
	and evalDict (ids: (ide * evT) list) (dc: dict) (r: evT env) : (ide * evT) list =
		match dc with
			Empty -> [] |
			Item(id, e1, tl) -> if memberDict id ids then failwith("id already exists") 
									else
										let value = (eval e1 r) in 
											(id, value)::(evalDict ((id, value)::ids) tl r)
	(* ritorna true se nel dizionario è presente la chiave key *)
	and memberDict (key: ide) (dc: (ide * evT) list) : bool =
		match dc with
			[] -> false |
			(k, v)::tl -> if k = key then true else memberDict key tl
	and convertKeyList (kl: keyList) : ide list =
		match kl with
			EndList -> [] |
			StrItem(id, tl) -> id::convertKeyList tl
	(* ritorna true se nella lista di chiavi è presente la chiave key *)
	and memberList (key: ide) (kl: keyList) : bool =
		match kl with
			EndList -> false |
			StrItem(k, tl) -> if k = key then true else memberList key tl
	(* inserisce key e newVal in coda al dizionario *)
	and insertDic (key: ide) (newVal: evT) (dc: (ide * evT) list) : (ide * evT) list =
		match dc with
			[] -> [(key, newVal)] |
			(k, v)::tl -> if key = k then failwith("this key already exists") else (k, v)::(insertDic key newVal tl)
	(* rimuove key dal dizinario *)
	and deleteFromDic (key: ide) (dc: (ide * evT) list) : (ide * evT) list =
		match dc with
			[] -> [] |
			(k, v)::tl -> if key = k then tl else (k,v)::(deleteFromDic key tl)
	(* chiama f per ogni elemento del dizionario passando l'elemento stesso come argomento *)	
	and iterDic (f: exp) (dc: dict) (r: evT env) : (ide * evT) list =
		match dc with
			Empty -> [] |
			Item(key, v, tl) -> let newVal = eval (FunCall(f, v)) r in (key, newVal)::(iterDic f tl r)
	(* applica una funzione ad ogni elemento e ritorna la somma di tutti i risultati *)
	and sumDic (f: exp) (dc: dict) (r: evT env) : evT =
		match dc with
			Empty -> Int(0) |
			Item(key, v, tl) -> let newVal = eval (FunCall(f, v)) r in sum newVal (sumDic f tl r)
	(* restituisce un dizionario con le sole chiavi contenute nella lista di chiavi *)
	and filterDic (kl: keyList) (dc: dict) (r: evT env) : (ide * evT) list =
		match dc with
			Empty -> [] |
			Item(key, v, tl) -> if (memberList key kl) then (key, (eval v r))::(filterDic kl tl r) else filterDic kl tl r;;