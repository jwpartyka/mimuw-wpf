type point = float * float
type kartka = point -> int

let prostokat ((a, b) : point) ((c, d) : point) =
    let inside ((x, y) : point) =
        a <= x && x <= c && b <= y && y <= d in
    ((fun p -> if inside p then 1 else 0) : kartka)

let kolko ((a, b) : point) (r : float) =
    let inside ((x, y) : point) =
        let (dx, dy) = (x -. a, y -. b) in
        dx *. dx +. dy *. dy <= r *. r in
    ((fun p -> if inside p then 1 else 0) : kartka)

let (-^) ((a, b) : point) ((c, d) : point) =
    ((a -. c, b -. d) : point)

let sq x = x *. x

let line (x1, y1) (x2, y2) =
    (y1 -. y2, x2 -. x1, x1 *. y2 -. x2 *. y1)

let reflect (p, q) p1 p2 =
    let (a, b, c) = line p1 p2 in
    let (a2, b2) = (sq a, sq b) in
    let x = (p *. (a2 -. b2) -. 2. *. b *. (a *. q +. c)) /. (a2 +. b2)
    and y = (q *. (b2 -. a2) -. 2. *. a *. (b *. p +. c)) /. (a2 +. b2) in
    (x, y)

let cross ((a, b) : point) ((c, d) : point) =
    a *. d -. b *. c

let zloz (p1 : point) (p2 : point) (k : kartka) =
    (fun (p : point) ->
        if cross (p2 -^ p1) (p -^ p1) <= 0. then k p
        else k p + k (reflect p p1 p2) : kartka)

let skladaj (l : (point * point) list) (k : kartka) =
    let pom (acc : kartka) ((p1, p2) : point * point) =
        zloz p1 p2 acc in
    List.fold_left pom k l
