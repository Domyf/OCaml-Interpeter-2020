(* creazione di un ambiente vuoto *)
let env0 = emptyenv Unbound;;

(* creazione di un dizionario vuoto *)
let emptyExpr = Dictionary(Empty);;
eval emptyExpr env0;;

(* creazione di un dizionario con valori *)
let dictExpr = Let("Magazzino", Dictionary(Item("mele", Eint(430), Item("banane", Eint(312), Item("arance", Eint(525), Item("pere", Eint(217), Empty))))), Den("Magazzino"));;
eval dictExpr env0;;

(* inserimento *)
let insertExpr = Insert("kiwi", Eint(300), dictExpr);;
eval insertExpr env0;;