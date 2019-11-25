type t =
    | Empty
    | Node of t * int * int * t * int * int

let empty = Empty

let is_empty t =
    t = Empty

let height = function
    | Node (_, _, _, _, h, _) -> h
    | Empty -> 0

let size = function
    | Node (_, _, _, _, _, sz) -> sz
    | Empty -> 0

let make l (a, b) r =
    if b < a then empty
    else
        let new_height = max (height l) (height r) + 1
        and new_size = size l + size r + b - a + 1 in
        Node (l, a, b, r, new_height, new_size)

exception Failure

let bal s =
    match s with
    | Empty -> Empty
    | Node (l, x, y, r, _, _) ->
        let hl = height l
        and hr = height r in
        if hl >= hr + 2 then
            match l with
            | Empty -> raise Failure
            | Node (ll, lx, ly, lr, _, _) ->
                if height ll >= height lr then
                    make ll (lx, ly) (make lr (x, y) r)
                else
                    match lr with
                    | Empty -> raise Failure
                    | Node (lrl, lrx, lry, lrr, _, _) ->
                        make (make ll (lx, ly) lrl) (lrx, lry) (make lrr (x, y) r)
        else if hr >= hl + 2 then
            match r with
            | Empty -> raise Failure
            | Node (rl, rx, ry, rr, _, _) ->
                if height rr >= height rl then
                    make (make l (x, y) rl) (rx, ry) rr
                else
                    match rl with
                    | Empty -> raise Failure
                    | Node (rll, rlx, rly, rlr, _, _) ->
                        make (make l (x, y) rll) (rlx, rly) (make rlr (rx, ry) rr)
        else s

let rec mem x s =
    match s with
    | Empty -> false
    | Node (l, a, b, r, _, _) ->
        if a <= x && x <= b then true
        else if x < a then mem x l
        else mem x r

let rec min_element s =
    match s with
    | Empty -> raise Failure
    | Node (Empty, x, y, _, _, _) -> (x, y)
    | Node (l, x, y, _, _, _) -> min_element l

let rec remove_min_element s =
    match s with
    | Empty -> raise Failure
    | Node (Empty, x, y, r, _, _) -> r
    | Node (l, x, y, r, _, _) -> make (remove_min_element l) (x, y) r |> bal

let rec max_element s =
    match s with
    | Empty -> raise Failure
    | Node (_, x, y, Empty, _, _) -> (x, y)
    | Node (_, x, y, r, _, _) -> max_element r

let rec remove_max_element s =
    match s with
    | Empty -> raise Failure
    | Node (l, x, y, Empty, _, _) -> l
    | Node (l, x, y, r, _, _) -> make l (x, y) (remove_max_element r) |> bal

(* (x, y) nie jest sąsiednie z drzewem *)

let rec add_one (x, y) = function
    | Empty ->
        make empty (x, y) empty
    | Node (l, a, b, r, _, _) ->
        if y < a then
            let new_l = add_one (x, y) l in
            make new_l (a, b) r |> bal
        else
            let new_r = add_one (x, y) r in
            make l (a, b) new_r |> bal

(* t1 i t2 nie są sąsiednie *)
let join t1 t2 =
    let rec pom l (x, y) r =
        match l, r with
        | Empty, _ -> add_one (x, y) r
        | _, Empty -> add_one (x, y) l
        | Node (ll, lx, ly, lr, lh, _), Node (rl, rx, ry, rr, rh, _) ->
            if rh > lh + 2 then
                make (pom l (x, y) rl) (rx, ry) rr |> bal
            else if lh > rh + 2 then
                make ll (lx, ly) (pom lr (x, y) r) |> bal
            else
                make l (x, y) r in

    match t1, t2 with
    | Empty, _-> t2
    | _, Empty -> t1
    | _ ->
        if height t1 < height t2 then
            let (x, y) = max_element t1 in
            pom (remove_max_element t1) (x, y) t2
        else
            let (x, y) = min_element t2 in
            pom t1 (x, y) (remove_min_element t2)

let below n s =
    let rec pom _n _s =
        match _s with
        | Empty -> 0
        | Node (l, a, b, r, _, _) ->
            if a <= _n && _n <= b then size l + _n - a + 1
            else if _n < a then pom _n l
            else size l + b - a + 1 + pom _n r in
    let res = pom n s in
    if res < 0 then max_int
    else res

let elements t =
    let rec pom acc s =
        match s with
        | Empty -> acc
        | Node (l, a, b, r, _, _) ->
            let new_acc = pom acc l in
            pom ((a, b) :: new_acc) r in
    List.rev (pom [] t)

let split x s =
    let rec loop = function
    | Empty -> (Empty, Empty)
    | Node (l, a, b, r, _, _) ->
        if a <= x && x <= b then
            if a = b then
                (l, r)
            else if a = x then
                (l, join (make empty (x + 1, b) empty) r)
            else if x = b then
                (join l (make empty (a, x - 1) empty), r)
            else
                (join l (make empty (a, x - 1) empty),
                join (make empty (x + 1, b) empty) r)
        else if x < a then
            let (t1, t2) = loop l in
            (t1, join t2 (join (make empty (a, b) empty) r))
        else
            let (t1, t2) = loop r in
            (join (join l (make empty (a, b) empty)) t1, t2) in
    let (t1, t2) = loop s in
    (t1, mem x s, t2)

let add (x, y) s =
    let (t1, _, _) = split x s
    and (_, _, t2) = split y s in
    match t1, t2 with
    | Empty, Empty ->
        make empty (x, y) empty
    | Empty, _ ->
        let (c, d) = min_element t2 in
        if y + 1 = c then
            join (make empty (x, d) empty) (remove_min_element t2)
        else
            join (make empty (x, y) empty) t2
    | _, Empty ->
        let (a, b) = max_element t1 in
        if b = x - 1 then
            join (remove_max_element t1) (make empty (a, y) empty)
        else
            join t1 (make empty (x, y) empty)
    | _ ->
        let (a, b) = max_element t1
        and (c, d) = min_element t2 in
        if b = x - 1 && c = y + 1 then
            let new_l = remove_max_element t1
            and new_r = remove_min_element t2 in
            join (join new_l (make empty (a, d) empty)) new_r
        else if b = x - 1 then
            join (join (remove_max_element t1) (make empty (a, y) empty)) t2
        else if y + 1 = c then
            join t1 (join (make empty (x, d) empty) (remove_min_element t2))
        else
            join (join t1 (make empty (x, y) empty)) t2

let remove (x, y) s =
    let (t1, _, _) = split x s
    and (_, _, t2) = split y s in
    match t1, t2 with
    | Empty, _ -> t2
    | _ ->
        let (a, b) = max_element t1 in
        join (join (remove_max_element t1) (make empty (a, b) empty)) t2


let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, x, y, r, _, _) -> loop l; f (x, y); loop r in
  loop set

let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, x, y, r, _, _) ->
          loop (f (x, y) (loop acc l)) r in
  loop acc set
