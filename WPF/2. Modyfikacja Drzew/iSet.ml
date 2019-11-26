(* Zadanie: Modyfikacja drzew       *)
(* Autor kodu: Janusz Partyka       *)
(* Code review: Szymon Dominikowski *)

(* Słowo 'drzewo' oznacza drzewo AVL reprezentujące pewien zbiór przedziałów.*)
(* Każde drzewo będące wynikiem procedury jest zrównoważone.                 *)
(* Wszystkie drzewa przekazywane do procedur, z wyjątkiem procedury balance, *)
(* są zrównoważone, czyli różnica pomiędzy wysokościami poddrzew każdego     *)
(* wierzchołka jest nie większa niż 2.                                       *)

(* Typ wariacyjny węzła drzewa. *)
type t =
    | Empty
    | Node of t * int * int * t * int * int

(* Pusty zbiór *)
let empty = Empty

(* Zwraca prawdę, jeżeli s jest pusty, fałsz w.p.p. *)
let is_empty s =
    s = Empty

(* Procedura zwracająca wysokość drzewa. *)
let height = function
    | Node (_, _, _, _, h, _) -> h
    | Empty -> 0

(* Procedura zwracająca liczbę elementów w zbiorze. *)
let size = function
    | Node (_, _, _, _, _, sz) -> sz
    | Empty -> 0

(* Procedura poprawiająca liczbę spoza zakresu inta. *)
let fix x =
    if x <= 0 then max_int
    else x

(* Procedura zwracająca liczbę liczb całkowitych w przedziale (a, b) *)
(* lub max_int, gdy liczba ta przekracza max_int.                    *)
let length a b =
    fix (b - a + 1)

(* Procedura tworząca węzeł, którego lewym poddrzewem jest l *)
(* kluczem jest (a, b), a prawym poddrzewem jest r.          *)
let make l (a, b) r =
    let new_height = max (height l) (height r) + 1
    and new_size = fix (fix (size l + length a b) + size r) in
    Node (l, a, b, r, new_height, new_size)

(* Procedura tworząca węzeł, którego poddrzewa są puste, a klucz to (a, b). *)
let leaf a b =
    make empty (a, b) empty

(* Procedura równoważąca drzewo s.                             *)
(* Zakłada, że wszystkie właściwe poddrzewa s są zrównoważone. *)
let balance s =
    match s with
    | Empty -> Empty
    | Node (l, x, y, r, _, _) ->
        let hl = height l
        and hr = height r in
        if hl > hr + 2 then
            match l with
            | Empty -> assert false
            | Node (ll, lx, ly, lr, _, _) ->
                if height ll >= height lr then
                    make ll (lx, ly) (make lr (x, y) r)
                else
                    match lr with
                    | Empty -> assert false
                    | Node (lrl, lrx, lry, lrr, _, _) ->
                        make (make ll (lx, ly) lrl) (lrx, lry)
                            (make lrr (x, y) r)
        else if hr > hl + 2 then
            match r with
            | Empty -> assert false
            | Node (rl, rx, ry, rr, _, _) ->
                if height rr >= height rl then
                    make (make l (x, y) rl) (rx, ry) rr
                else
                    match rl with
                    | Empty -> assert false
                    | Node (rll, rlx, rly, rlr, _, _) ->
                        make (make l (x, y) rll) (rlx, rly)
                            (make rlr (rx, ry) rr)
        else s

(* Procedura zwracająca prawdę, gdy x występuje w s, fałsz w.p.p. *)
let rec mem x s =
    match s with
    | Node (l, a, b, r, _, _) ->
        if a <= x && x <= b then true
        else if x < a then mem x l
        else mem x r
    | Empty -> false

(* Procedura zwracająca największy elemente w zbiorze s. *)
let rec min_element = function
    | Node (Empty, x, y, _, _, _) -> (x, y)
    | Node (l, x, y, _, _, _) -> min_element l
    | Empty -> raise Not_found

(* Procedura usuwająca najmniejszy element ze zbioru. *)
let rec remove_min_element = function
    | Node (Empty, x, y, r, _, _) -> r
    | Node (l, x, y, r, _, _) ->
        make (remove_min_element l) (x, y) r |> balance
    | Empty -> invalid_arg "iSet.remove_min_element"

(* Procedura zwracająca największy element w zbiorze s. *)
let rec max_element = function
    | Node (_, x, y, Empty, _, _) -> (x, y)
    | Node (_, x, y, r, _, _) -> max_element r
    | Empty -> raise Not_found

(* Procedura usuwająca największy element ze zbioru s. *)
let rec remove_max_element = function
    | Node (l, x, y, Empty, _, _) -> l
    | Node (l, x, y, r, _, _) ->
        make l (x, y) (remove_max_element r) |> balance
    | Empty -> invalid_arg "iSet.remove_max_element"

(* Procedura zwracająca zbioru s i przedziału (x, y).                       *)
(* (x, y) i żaden ze spójnych przedziałów (a, b) ze zbioru nie są sąsiednie *)
(* tzn.: y + 1 < a i b + 1 < x.                                             *)
let rec add_one (x, y) = function
    | Node (l, a, b, r, _, _) ->
        if y < a then
            make (add_one (x, y) l) (a, b) r |> balance
        else
            make l (a, b) (add_one (x, y) r) |> balance
    | Empty ->
        leaf x y

(* Procedura zwracająca drzewo, które jest złączeniem drzew l, r i (leaf x y) *)
(* Drzewa są podane w kolejności rosnących elementów i nie są one sąsiednie,  *)
(* tzn.: max_element l + 1 < x i y + 1 < min_element r.                       *)
let rec merge l (x, y) r =
    match l, r with
    | Empty, _ -> add_one (x, y) r
    | _, Empty -> add_one (x, y) l
    | Node (ll, lx, ly, lr, lh, _), Node (rl, rx, ry, rr, rh, _) ->
        if rh > lh + 2 then
            make (merge l (x, y) rl) (rx, ry) rr |> balance
        else if lh > rh + 2 then
            make ll (lx, ly) (merge lr (x, y) r) |> balance
        else
            make l (x, y) r

(* Procedura zwracająca liczbę liczb całkowitych w s nie większych niż n. *)
let rec below n s =
    match s with
    | Node (l, a, b, r, _, sz) ->
        if a <= n && n <= b then
            fix (size l + length a n)
        else if n < a then
            below n l
        else fix (fix (size l + length a b) + below n r)
    | Empty -> 0

(* Procedura zwracająca listę wszystkich spójnych przedziałów t *)
(* posortowanych rosnąco.                                       *)
let rec elements t =
    let rec pom acc s =
        match s with
        | Empty -> acc
        | Node (l, a, b, r, _, _) ->
            pom ((a, b) :: (pom acc r)) l in
    pom [] t

(* Procedura zwracająca trójkę (l, present, r), gdzie                       *)
(* l - zbiór elementów s mniejszych od x                                    *)
(* r - zbiór elementów s większych od x                                     *)
(* present - wartość logiczna będąca prawdą, gdy x jest w s, fałszem w.p.p. *)
let split x s =
    let rec loop = function
    | Empty -> (Empty, Empty)
    | Node (l, a, b, r, _, _) ->
        if a <= x && x <= b then
            if a = b then
                (l, r)
            else if a = x then
                (l, add_one (x + 1, b) r)
            else if x = b then
                (add_one (a, x - 1) l, r)
            else
                (add_one (a, x - 1) l, add_one (x + 1, b) r)
        else if x < a then
            let (t1, t2) = loop l in
            (t1, merge t2 (a, b) r)
        else
            let (t1, t2) = loop r in
            (merge l (a, b) t1, t2) in
    let (t1, t2) = loop s in
    (t1, mem x s, t2)

(* Procedura zwracająca sumę zbiorów (x, y) i s *)
let add (x, y) s =
    let (t1, _, _) = split x s
    and (_, _, t2) = split y s in
    match t1, t2 with
    | Empty, Empty ->
        leaf x y
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

(* Procedura zwracająca różnicę zbioru s i (x, y) *)
let remove (x, y) s =
    let (t1, _, _) = split x s
    and (_, _, t2) = split y s in
    match t1, t2 with
    | Empty, _ -> t2
    | _ ->
        let (a, b) = max_element t1 in
        merge (remove_max_element t1) (a, b) t2

(* Procedura wywołująca procedurę f na każdym elemencie zbioru s *)
let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, x, y, r, _, _) -> loop l; f (x, y); loop r in
  loop s

(* Procedura wykonująca fold left z procedurą f i akumulatorem początkowym a *)
(* na liście wszystkich spójnych przedziałów s posortowanych rosnąco         *)
let fold f s a =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, x, y, r, _, _) ->
          loop (f (x, y) (loop acc l)) r in
  loop a s
