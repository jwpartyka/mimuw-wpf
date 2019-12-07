(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrząszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Zadanie: Modyfikacja drzew       *)
(* Autor kodu: Janusz Partyka       *)
(* Code review: Szymon Dominikowski *)

(* Słowo 'drzewo' oznacza drzewo AVL reprezentujące pewien zbiór przedziałów.*)
(* Każde drzewo będące wynikiem procedury jest zrównoważone.                 *)
(* Wszystkie drzewa przekazywane do procedur, z wyjątkiem procedury balance, *)
(* są zrównoważone, czyli różnica pomiędzy wysokościami poddrzew każdego     *)
(* wierzchołka jest nie większa niż 2.                                       *)

(* Typ przedziałów liczb *)
type interval = int * int

(* Typ wariacyjny węzła drzewa. *)
type t =
    (* Pusty węzęł                                                       *)
    | Empty
    (* (lewe poddrzewo, klucz (x, y), prawe poddrzewo, wysokość, rozmiar *)
    | Node of t * interval * t * int * int

(* Pusty zbiór *)
let empty =
    Empty

(* Zwraca prawdę, jeżeli s jest pusty, fałsz w.p.p. *)
let is_empty s =
    s = Empty

(* Procedura zwracająca wysokość drzewa. *)
let height = function
    | Node (_, _, _, h, _) -> h
    | Empty -> 0

(* Procedura zwracająca liczbę elementów w zbiorze. *)
let size = function
    | Node (_, _, _, _, sz) -> sz
    | Empty -> 0

(* Procedura poprawiająca liczbę spoza zakresu inta.                       *)
(* Jest wywoływana od razu po wykonaniu operacji dodawania/odejmowania     *)
(* liczb, których suma/różnica mogła wyjść poza zakres inta.               *)
let fix x =
    if x <= 0 then max_int
    else x

(* Procedura zwracająca liczbę liczb całkowitych w przedziale [a, b] *)
(* lub max_int, gdy liczba ta przekracza max_int.                    *)
let length a b =
    fix (b - a + 1)

(* Procedura tworząca węzeł, którego lewym poddrzewem jest l *)
(* kluczem jest [a, b], a prawym poddrzewem jest r.          *)
let make l (a, b) r =
    let new_height = max (height l) (height r) + 1
    and new_size = fix (fix (size l + length a b) + size r) in
    Node (l, (a, b), r, new_height, new_size)

(* Procedura tworząca węzeł, którego poddrzewa są puste, a klucz to k. *)
let leaf k =
    make empty k empty

(* Procedura równoważąca drzewo s.                             *)
(* Zakłada, że wszystkie właściwe poddrzewa s są zrównoważone. *)
let balance l k r =
    let hl = height l
    and hr = height r in
    if hl > hr + 2 then
        match l with
        | Empty -> assert false
        | Node (ll, lk, lr, _, _) ->
            if height ll >= height lr then
                make ll lk (make lr k r)
            else
                match lr with
                | Empty -> assert false
                | Node (lrl, lrk, lrr, _, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
    else if hr > hl + 2 then
        match r with
        | Empty -> assert false
        | Node (rl, rk, rr, _, _) ->
            if height rr >= height rl then
                make (make l k rl) rk rr
            else
                match rl with
                | Empty -> assert false
                | Node (rll, rlk, rlr, _, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
    else make l k r

(* Procedura zwracająca prawdę, gdy x występuje w s, fałsz w.p.p. *)
let rec mem x s =
    match s with
    | Node (l, (a, b), r, _, _) ->
        if a <= x && x <= b then true
        else if x < a then mem x l
        else mem x r
    | Empty -> false

(* Procedura zwracająca największy elemente w zbiorze. *)
let rec min_element = function
    | Node (Empty, k, _, _, _) -> k
    | Node (l, _, _, _, _) -> min_element l
    | Empty -> raise Not_found

(* Procedura usuwająca najmniejszy element ze zbioru. *)
let rec remove_min_element = function
    | Node (Empty, _, r, _, _) -> r
    | Node (l, k, r, _, _) ->
        balance (remove_min_element l) k r
    | Empty -> invalid_arg "ISet.remove_min_element"

(* Procedura zwracająca największy element w zbiorze. *)
let rec max_element = function
    | Node (_, k, Empty, _, _) -> k
    | Node (_, _, r, _, _) -> max_element r
    | Empty -> raise Not_found

(* Procedura usuwająca największy element ze zbioru. *)
let rec remove_max_element = function
    | Node (l, _, Empty, _, _) -> l
    | Node (l, k, r, _, _) ->
        balance l k (remove_max_element r)
    | Empty -> invalid_arg "ISet.remove_max_element"

(* Procedura zwracająca sumę zbioru s i przedziału [x, y].                  *)
(* [x, y] i żaden ze spójnych przedziałów [a, b] ze zbioru nie są sąsiednie *)
(* tzn.: y + 1 < a i b + 1 < x.                                             *)
(* Zakłada, że x <= y                                                       *)
let rec add_one (x, y) s =
    match s with
    | Node (l, (a, b), r, _, _) ->
        if y < a then
            balance (add_one (x, y) l) (a, b) r
        else
            balance l (a, b) (add_one (x, y) r)
    | Empty ->
        leaf (x, y)

(* Procedura zwracająca drzewo, które jest złączeniem drzew l, r i (leaf k), *)
(* gdzie k = [x, y]                                                          *)
(* Drzewa są podane w kolejności rosnących elementów i nie są one sąsiednie, *)
(* tzn.: max_element l + 1 < x i y + 1 < min_element r.                      *)
let rec merge l k r =
    match l, r with
    | Empty, _ -> add_one k r
    | _, Empty -> add_one k l
    | Node (ll, lk, lr, lh, _), Node (rl, rk, rr, rh, _) ->
        if rh > lh + 2 then
            balance (merge l k rl) rk rr
        else if lh > rh + 2 then
            balance ll lk (merge lr k r)
        else
            make l k r

(* Procedura zwracająca trójkę (l, present, r), gdzie                       *)
(* l - zbiór elementów s mniejszych od x                                    *)
(* r - zbiór elementów s większych od x                                     *)
(* present - wartość logiczna będąca prawdą, gdy x jest w s, fałszem w.p.p. *)
let split x s =
    let rec loop = function
    | Empty -> (Empty, Empty)
    | Node (l, (a, b), r, _, _) ->
        if x < a then
            let (t1, t2) = loop l in
            (t1, merge t2 (a, b) r)
        else if x > b then
            let (t1, t2) = loop r in
            (merge l (a, b) t1, t2)
        else if a < x && x < b then
            (add_one (a, x - 1) l, add_one (x + 1, b) r)
        else if a = b && a = x then
            (l, r)
        else if a = x then
            (l, add_one (x + 1, b) r)
        else
            (add_one (a, x - 1) l, r) in
    let (t1, t2) = loop s in
    (t1, mem x s, t2)

(* Procedura zwracająca sumę zbiorów [x, y] i s *)
(* Zakłada, że x <= y                           *)
let add (x, y) s =
    let (t1, _, _) = split x s
    and (_, _, t2) = split y s in
    match t1, t2 with
    | Empty, Empty ->
        leaf (x, y)
    | Empty, _ ->
        let (c, d) = min_element t2 in
        if y + 1 = c then
            add_one (x, d) (remove_min_element t2)
        else
            add_one (x, y) t2
    | _, Empty ->
        let (a, b) = max_element t1 in
        if b = x - 1 then
            add_one (a, y) (remove_max_element t1)
        else
            add_one (x, y) t1
    | _ ->
        let (a, b) = max_element t1
        and (c, d) = min_element t2 in
        if b = x - 1 && c = y + 1 then
            merge (remove_max_element t1) (a, d) (remove_min_element t2)
        else if b = x - 1 then
            merge (remove_max_element t1) (a, y) t2
        else if y + 1 = c then
            merge t1 (x, d) (remove_min_element t2)
        else
            merge t1 (x, y) t2

(* Procedura zwracająca różnicę zbioru s i [x, y] *)
(* Zakłada, że x <= y                             *)
let remove (x, y) s =
    let (t1, _, _) = split x s
    and (_, _, t2) = split y s in
    match t1, t2 with
    | Empty, _ -> t2
    | _ ->
        let (a, b) = max_element t1 in
        merge (remove_max_element t1) (a, b) t2

(* Procedura zwracająca liczbę liczb całkowitych w s nie większych niż n. *)
let below n s =
    let (l, pres, _) = split n s in
    if pres = true then fix (size l + 1)
    else size l

(* Procedura zwracająca listę wszystkich spójnych przedziałów t *)
(* posortowanych rosnąco.                                       *)
let elements d =
    let rec pom acc s =
        match s with
        | Empty -> acc
        | Node (l, k, r, _, _) ->
            pom (k :: (pom acc r)) l in
    pom [] d

(* Procedura wywołująca procedurę f na każdym elemencie zbioru s *)
let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop s

(* Procedura wykonująca fold left z procedurą f i akumulatorem początkowym a *)
(* na liście wszystkich spójnych przedziałów s posortowanych rosnąco         *)
let fold f s a =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop a s

(* TESTY: *)
(*
let a = empty
let a = add (-20, 5) a
let a = add (6, 18) a
let a = add (4, 10) a
let a = add (14, 16) a
let a = remove (-18, 14) a
let a = remove (5, 17) a;;
assert(mem 14 a = false);;
let a = add (-4, 9) a;;
assert(mem 16 a = false);;
assert(mem (-14) a = false);;
assert(mem 10 a = false);;
let a = remove (-9, 10) a;;
let a = add (-6, 7) a;;
let a = add (-2, 7) a;;
let a = add (-12, 17) a;;
let a = add (-13, 8) a;;
let a = add (-13, -2) a;;
assert(mem 11 a = true);;
assert(elements a = [(-20, -19); (-13, 18)]);;
*)
