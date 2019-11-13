(* Zadanie: Arytmetyka           *)
(* Autor: Janusz Partyka         *)
(* Code review: Jakub Wasilewski *)

open List

(* Typ reprezentujacy jednoczesciowy zbior liczb. *)
type interval = {l : float; r : float}

(* Typ reprezentujacy niedokladne wartosci. *)
type wartosc =
    | Pusty                           (* Zbior pusty.          *)
    | Jeden of interval               (* Zbior jednoczesciowy. *)
    | Dwa of interval * interval      (* Zbior dwuczesciowy.   *)

(* Procedura, ktora poprawi prawy koniec przedzialu a na -0., *)
(* jezeli jest on rowny 0..                                   *)
(* a jest typu interval.                                      *)
let popraw_zero a =
    if a.r = 0. then Jeden {a with r = -0.}
    else Jeden a

(* wartosc_od_do x y = [x; y] *)
(* war. pocz.: x <= y         *)
let wartosc_od_do x y =
    popraw_zero {l = x; r = y}

(* wartosc_dokladnosc x p = [x - p% z x; x + p% z x] *)
(* war. pocz.: p > 0                                 *)
let wartosc_dokladnosc x p =
    let procent = abs_float (p /. 100. *. x) in
    wartosc_od_do (x -. procent) (x +. procent)

(* wartosc dokladna x = [x; x] *)
let wartosc_dokladna x =
    Jeden {l = x; r = x}

(* in_wartosc x y = wartosc x moze byc rowna y *)
let in_wartosc x y =
    match x with
    | Jeden a -> a.l <= y && y <= a.r
    | Dwa (a, b) -> (a.l <= y && y <= a.r) || (b.l <= y && y <= b.r)
    | Pusty -> false

(* min_wartosc x = najmniejsza mozliwa wartosc x     *)
(* lub neg_infinity jesli brak dolnego ograniczenia. *)
let min_wartosc x =
    match x with
    | Jeden a -> a.l
    | Dwa (a, _) -> a.l
    | Pusty -> nan

(* max_wartosc x = najwieksza mozliwa wartosc x, *)
(* lub infinity jesli brak gornego ograniczenia. *)
let max_wartosc x =
    match x with
    | Jeden a -> a.r
    | Dwa (_, b) -> b.r
    | Pusty -> nan

(* sr_wartosc x = srednia arytmetyczna min_wartosc x i max_wartosc x *)
(* lub nan jesli min_wartosc x i max_wartosc x nie sa skonczone.     *)
let sr_wartosc x =
    if x = Pusty then nan
    else
        let (mini, maks) = (min_wartosc x, max_wartosc x) in
        if mini = neg_infinity && maks = infinity then nan
        else (mini +. maks) /. 2.

(* union x y = niedokladna wartosc reprezentujaca sume zbiorow *)
(* niedokladnych wartosci x i y.                               *)
let rec union x y =
    (* pom a b = union (Jeden a) (Jeden b) *)
    (* a i b sa typu interval.             *)
    let rec pom a b =
        if b.l < a.l then pom b a
        else if a.l <= b.r && b.l <= a.r then
            Jeden {l = min a.l b.l; r = max a.r b.r}
        else Dwa (a, b) in
    match x, y with
    | Jeden a, Jeden b -> pom a b
    | Jeden a, Dwa (c, d) -> union (union x (Jeden c)) (Jeden d)
    | Dwa (a, b), Jeden c -> union (Jeden a) (union (Jeden b) (Jeden c))
    | Dwa (a, b), Dwa (c, d) ->
        union (union (Jeden a) (Jeden c)) (union (Jeden b) (Jeden d))
    | Pusty, _ -> y
    | _, Pusty -> x

(* Procedura pomocnicza do procedur plus/minus/razy/podzielic, *)
(* ktora aplikuje procedure f na parach jednoczesciowych       *)
(* zbiorow, takich, ze jeden zawiera sie w x, a drugi w y.     *)
(* x, y - wartosci niedokladne.    .                           *)
let rec zaaplikuj f x y =
    match x, y with
    | Jeden a, Jeden c -> f a c
    | Jeden a, Dwa (c, d) ->
        union (zaaplikuj f x (Jeden c)) (zaaplikuj f x (Jeden d))
    | Dwa (a, b), _ ->
        union (zaaplikuj f (Jeden a) y) (zaaplikuj f (Jeden b) y)
    | _, _ -> Pusty

(* Procedura wykonujaca operacje plus/minus/razy/podzielic *)
(* na parze jednoczesciowych zbiorow a i b.                *)
(* f jest jedna z procedur: (+.), (-.), ( *. ), (/.).      *)
(* a i b sa typu interval.                                 *)
let operacja f a b =
    let konce = [f a.l b.l; f a.l b.r; f a.r b.l; f a.r b.r] in
    let ok_konce = filter (fun x -> classify_float x <> FP_nan) konce in
    if ok_konce = [] then Jeden {l = 0.; r = 0.}
    else
        let mini = fold_left min infinity ok_konce
        and maks = fold_left max neg_infinity ok_konce in
        popraw_zero {l = mini; r = maks}

(* plus x y = {a + b: in_wartosc x a i in_wartosc y b} *)
let plus x y =
    zaaplikuj (operacja (+.)) x y

(* minus x y = {a - b: in_wartosc x a i in_wartosc y b} *)
let minus x y =
    zaaplikuj (operacja (-.)) x y

(* razy x y = {a * b: in_wartosc x a i in_wartosc y b} *)
let razy x y =
    zaaplikuj (operacja ( *. )) x y

(* podzielic x y = {a / b: in_wartosc x a i in_wartosc y b} *)
let podzielic x y =
    (* pom a b = podzielic (Jeden a) (Jeden b)                     *)
    (* a i b sa typu interval.                                     *)
    (* Jezeli, zbior (l, r) zawiera i ujemne, i dodatnie wartosci, *)
    (* to zostaje on podzielony na dwa zbiory: (l; -0.) i (0.; r). *)
    let rec pom a b =
        if b = {l = 0.; r = 0.} then Pusty
        else if a.l *. a.r < 0. then
            union (pom {a with r = -0.} b) (pom {a with l = 0.} b)
        else if b.l *. b.r < 0. then
            union (pom a {b with r = -0.}) (pom a {b with l = 0.})
        else
            operacja (/.) a b in
    zaaplikuj pom x y

(*

(* Testy autorstwa Mateusza Gienieczki *)

let eps = 1e-6;;

let a = wartosc_od_do 3. 7.;;                        (* [3., 7.]                      *)

assert(min_wartosc a = 3.0);;
assert(max_wartosc a = 7.0);;
assert(in_wartosc a 4.);;
assert(not (in_wartosc a 2.));;

let b = wartosc_od_do (-2.) 5.;;                     (* [-2., 5.]                     *)

assert(sr_wartosc b = 1.5);;
assert(min_wartosc b = -2.);;
assert(max_wartosc b = 5.);;
assert(in_wartosc b (-0.));;

let c = podzielic a b;;                              (* [-inf, -1.5] U [0.6, inf]     *)

assert(not (in_wartosc c 0.));;
assert(in_wartosc c 100.);;

let d = podzielic c b;;                              (* [-inf, -0.3] U [0.12, inf]    *)

assert(compare (sr_wartosc d) nan = 0);;
assert(in_wartosc d (-3. /. 10. -. eps));;
assert(not (in_wartosc d (-3. /. 10. +. eps)));;
assert(max_wartosc d = infinity);;

*)
