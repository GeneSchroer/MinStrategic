
(*In order to find the minimum Strategic set, we break the program
 into multiple steps*)

(* First, we need to create a number of support functions *)

(* Find if an element X is in list L *)
fun member(X, L) = if L = [] then false
else if X = hd(L) then true
else member(X, tl(L));

(* Return the union of two sets L1 and L2 *)
fun union(L1, L2) = if L1 = [] then L2
else if member(hd(L1), L2) then union(tl(L1), L2)
else hd(L1)::union(tl(L1),L2);

(* Remove a single element X from list L *)
fun removeOne(X, L) = if L = [] then []
else if X = hd(L) then tl(L)
else hd(L)::removeOne(X, tl(L));

(* Check sets L1 and L2 are equal *)
fun equal(L, M) = if L = [] then true
else if member(hd(L),M) then equal(tl(L), removeOne(hd(L),M))
else false;

(* Determine if set L1 is a subset of set L2 *)
fun subset(L1, L2) = if L1 = [] then true
else if member(hd(L1), L2) then subset(tl(L1), L2)
else false;

(* Second, get the list of all goods 
 produced by the companies in the set *)

fun getGoods([]) = []
| getGoods( (X,Y,Z)::Xs) = union(getGoods(Xs), #2(X,Y,Z));


(* Third, get the power sets of all companies *)

(* Extract the list of companies out of the strategic set *)
fun getCompanyList([]) = []
| getCompanyList((X,Y,Z)::Xs) = [#1(X,Y,Z)] @ getCompanyList(Xs);

fun getCompanyList2(L) = if L = [] then []
else getCompanyList(hd(L))::getCompanyList2(tl(L));

(* Insert an element E into every list in L*)
fun insert_all(E,L) = if L = [] then []
else (E::hd(L)) :: insert_all(E,tl(L));

(* Generate the power set of a list of companies *)

fun powerSet(L) = 
if L = [] then [[]]
else powerSet(tl(L)) @ insert_all(hd(L), powerSet(tl(L)));


val m1 = [1, 2, 3];
val m2 = [3, 2, 1];

equal(m1, m2);

(* Third, add any additional companies owned by companies in each set *)

(* Helper function that may add companies to a list *)
fun addComp_helper(P:(string * string list *string list) list, O:(string * string list * string list)list) = 
if O = [] then []
else if #3(hd(O)) = [] then union(P, addComp_helper(P,tl(O)))
else if subset(#3(hd(O)),getCompanyList(P)) 
  then union(union(P,[hd(O)]), addComp_helper(P, tl(O)))
else union(P, addComp_helper(P,tl(O)));

(* Adds any additional companies to each set in the powerset  *)
fun addComp(P, O) = 
if P = [] then []
else addComp_helper(hd(P),O) ::  addComp(tl(P),O);

(* Fourth, remove all companies in the powerset 
that do not produce all goods *)

fun eliminateFailure(G, P) = if P = [] then []
else if equal(G, getGoods(hd(P))) then hd(P)::eliminateFailure(G, tl(P))
else eliminateFailure(G, tl(P));


(* Fifth, determine the minimum set *)

fun length(M, L) = if L = [] then M
else length(M+1, tl(L));

fun min(M, L, S) = if L = [] then S
else if length(0, hd(L)) < M then min(length(0,hd(L)), tl(L), hd(L))
else min(M, tl(L), S);

(* Finally, put it all together *)
fun getMinimumSet(L: (string * string list * string list) list) =
if L = [] then []
else [];

val companyList = [("traderJoes", ["pasta", "meat"], ["aldi"]), ("aldi",[],[]), ("costco", ["water", "tires", "icecream"],[])];

val goods = getGoods(companyList);
val temp = powerSet(companyList);
val powerList = addComp(temp, companyList);
val trimmedList = eliminateFailure(goods, powerList);

val trimmedComp = getCompanyList2(trimmedList);
val c = getCompanyList(companyList);

val minSet = min(length(0, getCompanyList(companyList)), trimmedComp, []); 
