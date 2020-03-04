(* Zadanie: Origami           *)
(* Autor: Janusz Partyka      *)
(* Code review: Marcin Brojek *)

(* Typ punktu na płaszczyźnie *)
type point = float * float

(* Typ poskładanej kartki: ile razy kartkę przebije szpilka wbita *)
(* w danym punkcie                                                *)
type kartka = point -> int

(* Epsilon do radzenia sobie z błędem precyzji floatów *)
let eps = 1e-9

(* Procedura zwracająca x^2 *)
let square x = x *. x

(* Wyjątek podnoszony, gdy rogi prostokąta są w złym porządku *)
exception Bad_rectangle

(* [prostokat p1 p2] zwraca kartkę, reprezentująca domknięty prostokąt       *)
(* o bokach równoległych do osi ukłądu współrzędnych i lewym dolnym rogu     *)
(* [p1] a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo      *)
(* i w dół od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz        *)
(* (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz, w.p.p. 0 *)
let prostokat ((a, b) : point) ((c, d) : point) =
    let inside (x, y) =
        a -. x <= eps && x -. c <= eps && b -. y <= eps && y -. d <= eps in
    if c < a || d < b then raise Bad_rectangle
    else (fun p -> if inside p then 1 else 0 : kartka)

(* Wyjątek podnoszony, gdy długość promienia koła jest niedodatnia. *)
exception Bad_circle

(* [kolko p r] zwraca kartkę reprezentującą kółko domknięte o środku      *)
(* w punkcie [(a, b)] i promieniu [r]. Gdy w kartkę tę wbije się szpilkę  *)
(* wewnątrz (lub na brzegu) koła, kartka zostanie przebita 1 raz, w.p.p 0 *)
let kolko ((a, b) : point) r =
    let inside (x, y) =
        square (x -. a) +. square (y -. b) -. square r <= eps in
    if r <= 0. then raise Bad_circle
    else (fun p -> if inside p then 1 else 0 : kartka)

(* [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez punkty *)
(* [p1] i [p2] (muszą to być różne punkty). Papier jest składany w ten        *)
(* sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])    *)
(* jest przekładany na lewą, Wynikiem procedury jest złożona kartka.          *)
(* jej przebicie po prawej stronie prostej powinno więc zwrócić 0. Przebicie  *)
(* dokładnie na prostej powinno zwrócić tyle samo, co przebicie przed         *)
(* złożeniem. Po stronie lewej - tyle co przed złożeniem plus przebicie       *)
(* rozłożonej kartki w punkcie, który nałożył się na punkt przebicia.         *)
let zloz (p1 : point) (p2 : point) (k : kartka) =
    (* Operator odejmujący dwa punkty *)
    let (-^) (a, b) (c, d) =
        ((a -. c, b -. d) : point) in

    (* Procedura zwracająca współczynniki a, b, c prostej ax + by + c = 0 *)
    (* przechodzącej przez punkt (x1, y1) (x2, y2)                        *)
    let line (x1, y1) (x2, y2) =
        (y1 -. y2, x2 -. x1, x1 *. y2 -. x2 *. y1) in

    (* Procedura zwracająca odbicie punktu (p, q) względem prostej *)
    (* przechodzącej przez punkty p1 i p2                          *)
    let reflect (p, q) p1 p2 =
        let (a, b, c) = line p1 p2 in
        let (a2, b2) = (square a, square b) in
        let x = (p *. (b2 -. a2) -. 2. *. a *. (b *. q +. c)) /. (a2 +. b2)
        and y = (q *. (a2 -. b2) -. 2. *. b *. (a *. p +. c)) /. (a2 +. b2) in
        (x, y) in

    (* Procedura zwracająca iloczyn wektorowy wektorów [a, b] i [c, d] *)
    let cross (a, b) (c, d) =
        a *. d -. b *. c in

    (fun (p : point) ->
        let cr = cross (p2 -^ p1) (p -^ p1) in
        if abs_float cr <= eps then k (reflect p p1 p2)
        else if cr > 0. then k p + k (reflect p p1 p2)
        else 0 : kartka)

(* [skladaj [(p1_1, p2_1); ...; (p1_n, p2_n)] k =                           *)
(*     zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]                     *)
(* czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż prostych z listy. *)
let skladaj (l : (point * point) list) (k : kartka) =
    List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l
