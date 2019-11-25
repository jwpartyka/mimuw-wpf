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

let rec bal s =
    let left_rotation = function
    | Empty -> raise Failure
    | Node (l, x, y, r, _, _) ->
        match r with
        | Empty -> raise Failure
        | Node (rl, rx, ry, rr, _, _) ->
            make (make l (x, y) rl) (rx, ry) rr in

    let right_rotation = function
    | Empty -> raise Failure
    | Node (l, x, y, r, _, _) ->
        match l with
        | Empty -> raise Failure
        | Node (ll, lx, ly, lr, _, _) ->
            make ll (lx, ly) (make lr (x, y) r) in

    match s with
    | Empty -> raise Failure
    | Node (l, x, y, r, _, _) ->
        let hl = height l
        and hr = height r in
        if hl >= hr + 2 then
            match l with
            | Empty -> raise Failure
            | Node (ll, lx, ly, lr, _, _) ->
                if height ll >= height lr then
                    right_rotation s
                else
                    make (left_rotation l) (x, y) r |> bal
        else if hr >= hl + 2 then
            match r with
            | Empty -> raise Failure
            | Node (rl, _, _, rr, _, _) ->
                if height rl = height rr + 1 then
                    make l (x, y) (right_rotation r) |> bal
                else left_rotation s
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

let rec merge t1 t2 =
    let rec merge_left (x, y) s t =
        match t with
        | Empty -> raise Failure
        | Node (l, a, b, r, _, _) ->
            if height l <= height s + 1 then
                make (make s (x, y) l) (a, b) r |> bal
            else
                make (merge_left (x, y) s l) (a, b) r |> bal in
    let rec merge_right (x, y) s t =
        match t with
        | Empty -> raise Failure
        | Node (l, a, b, r, _, _) ->
            if height r <= height s + 1 then
                make l (a, b) (make r (x, y) s) |> bal
            else
                make l (a, b) (merge_right (x, y) s r) |> bal in


    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
        let (a, b) = max_element t1
        and (c, d) = min_element t2 in
        if b > c then merge t2 t1 else
        if height t1 < height t2 then
            let (x, y) = max_element t1 in
            let s = remove_max_element t1 in
            merge_left (x, y) s t2
        else
            let (x, y) = min_element t2 in
            let s = remove_min_element t2 in
            merge_right (x, y) s t1

let rec remove (x, y) s =
    if y < x then s
    else
        match s with
        | Empty -> Empty
        | Node (l, a, b, r, _, _) ->
            if y < a then
                let new_l = remove (x, y) l
                and new_r = merge (make empty (a, b) empty) r in
                merge new_l new_r
            else if b < x then
                let new_l = merge l (make empty (a, b) empty)
                and new_r = remove (x, y) r in
                merge new_l new_r
            else
                let new_l = merge (remove (x, a - 1) l)
                    (make empty (a, x - 1) empty)
                and new_r = merge (make empty (y + 1, b) empty)
                    (remove (b + 1, y) r) in
                merge new_l new_r

let rec add (x, y) s =
    let rec fix t =
        match t with
        | Empty -> Empty
        | Node (Empty, a, b, Empty, _, _) -> t
        | Node (Empty, a, b, r, _, _) ->
            let (e, f) = min_element r in
            if b + 1 = e then
                make empty (a, f) (remove_min_element r) |> bal
            else t
        | Node (l, a, b, Empty, _, _) ->
            let (c, d) = max_element l in
            if d = a - 1 then
                make (remove_max_element l) (c, b) empty |> bal
            else t
        | Node (l, a, b, r, _, _) ->
            let (c, d) = max_element l in
            if d = a - 1 then
                make (remove_max_element l) (c, b) r |> bal |> fix
            else
                let (e, f) = min_element r in
                if b + 1 = e then
                    make l (a, f) (remove_min_element r) |> bal
                else t in

    if y < x then s
    else
        match s with
        | Empty -> make empty (x, y) empty
        | Node (l, a, b, r, _, _) ->
            if y < a then
                make (add (x, y) l) (a, b) r |> bal |> fix
            else if b < x then
                make l (a, b) (add (x, y) r) |> bal |> fix
            else if a <= x && y <= b then s
            else if x < a && y <= b then
                make (remove (x, a - 1) l) (x, b) r |> bal |> fix
            else if a <= x && b < y then
                make l (a, y) (remove (b + 1, y) r) |> bal |> fix
            else
                make (remove (x, a - 1) l) (x, y) (remove (b + 1, y) r) |> bal
                    |> fix

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
    let rec lesser acc y t =
        match t with
        | Empty -> acc
        | Node (l, a, b, r, _, _) ->
            if a <= y && y <= b then
                merge acc (merge l (make empty (a, y - 1) empty))
            else if y < a then
                lesser acc y l
            else
                merge (merge l (make empty (a, b) empty)) (lesser acc y r) in
    let rec greater acc y t =
        match t with
        | Empty -> acc
        | Node (l, a, b, r, _, _) ->
            if a <= y && y <= b then
                merge acc (merge (make empty (y + 1, b) empty) r)
            else if b < y then
                greater acc y r
            else
                merge (greater acc y l) (merge (make empty (a, b) empty) r) in
    (lesser empty x s, mem x s, greater empty x s)

    (* Autor: Mateusz Gienieczko *)

    let info = false;;

    let simple l =
      let (e, res) =
        List.fold_left (fun ((px, py), la) (x, y) ->
          if py + 1 >= x then ((px, max py y), la)
          else ((x, y), (px, py)::la)) ((List.hd l), []) (List.tl l)
      in
      List.rev (e::res);;

    let long l =
      let rec add_inter acc (x, y) =
        if x == y then x::acc
        else add_inter (x::acc) (x + 1, y)
      in
      List.rev (List.fold_left (fun acc inter -> (add_inter [] inter) @ acc) [] l);;

    let add_list =
      List.fold_left (fun s x -> add x s);;

    let mem_all a l1 =
      List.filter (fun x -> not (mem x a)) l1 = []

    let mem_none a l1 =
      List.filter (fun x -> mem x a) l1 = []

    (* Small correctness tests *)

    let l1 = [(-10, -8); (-7, -7); (-4, -1); (1, 1); (3, 7); (10, 15); (100, 1000)];;
    let a = add_list empty l1;;

    assert(elements a = simple l1);;
    assert(mem_all a (long l1));;
    assert(below 1000 a = 921);;

    let (a1, b, a2) = split 4 a;;
    assert(b);;
    assert(simple (elements a1 @ [(4, 4)] @ elements a2) = simple l1);;
    assert(List.filter (fun (x, y) -> y >= 4) (elements a1) = []);;
    assert(List.filter (fun (x, y) -> x <= 4) (elements a2) = []);;

    let (a1, b, a2) = split 3 a;;
    assert(b);;
    assert(simple (elements a1 @ [(3, 3)] @ elements a2) = simple l1);;
    assert(List.filter (fun (x, y) -> y >= 3) (elements a1) = []);;
    assert(List.filter (fun (x, y) -> x <= 3) (elements a2) = []);;

    let (a1, b, a2) = split 2 a;;
    assert(not b);;
    assert(simple(elements a1 @ elements a2) = simple l1);;
    assert(List.filter (fun (x, y) -> y >= 2) (elements a1) = []);;
    assert(List.filter (fun (x, y) -> x <= 2) (elements a2) = []);;

    let b = add (1, 10) a;;
    let l2 = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) ((1, 10)::l1);;

    assert(elements b = simple l2);;

    let c = remove (1, 10) a;;
    let d = remove (1, 10) b;;

    assert(elements c = elements d);;

    let e = add (min_int, max_int) a;;
    assert(elements e = [(min_int, max_int)]);;
    assert(below 1 e = max_int);;

    let f = remove (min_int, max_int) a;;
    assert(elements f = []);;

    let l3 = [(16, 99); (2, 2); (8, 9); (-6, -5)];;
    let g = add_list a l3;;
    assert(elements g = [(-10, -1); (1, 1000)]);;
    assert(not (mem 0 g));;
    let h = remove (420, 690) g;;
    assert(not (mem 500 h));;
    assert(elements h = [(-10, -1); (1, 419); (691, 1000)]);;
    let i = add (0, 0) g;;
    assert(elements i = [(-10, 1000)]);;
    let j = remove (-9, -1) i;;
    assert(elements j = [(-10, -10); (0, 1000)]);;
    let k = remove (500, 999) j;;
    assert(elements k = [(-10, -10); (0, 499); (1000, 1000)]);;

    (* Performance tests *)

    let rec aux l i =
      if i = 0 then l
      else aux (i::l) (i - 1);;

    let l1 = snd (List.fold_left (fun (i, l) _ -> (i + 3, (i, i + 1)::l)) (min_int, [])
        (aux [] 100000));;

    let l2 = snd (List.fold_left (fun (i, l) _ -> (i - 3, (i, i + 1)::l)) (max_int - 3, [])
    	(aux [] 100000));;

    let l3 = snd (List.fold_left (fun (i, l) _ -> (i + 3, (i, i + 1)::l)) (0, [])
    	(aux [] 100000));;

    let l4 = snd (List.fold_left (fun (i, l) _ -> (i - 3, (i, i + 1)::l)) (0, [])
    	(aux [] 100000));;

    if info then Pervasives.print_endline "Starting performence";;
    let a = add_list empty l1;;
    if info then Pervasives.print_endline "Added l1";;
    let a = add_list a l1;;
    if info then Pervasives.print_endline "Added l1";;
    let a = add_list a l2;;
    if info then Pervasives.print_endline "Added l2";;
    let a = add_list a l2;;
    if info then Pervasives.print_endline "Added l2";;
    let a = add_list a l3;;
    if info then Pervasives.print_endline "Added l3";;
    let a = add_list a l3;;
    if info then Pervasives.print_endline "Added l3";;
    let a = add_list a l4;;
    if info then Pervasives.print_endline "Added l4";;
    let a = add_list a l4;;
    if info then Pervasives.print_endline "Added l4";;

    let test s (a, b) step i =
      let rec aux s (x, y) i =
        if i = 0 then s
        else aux (remove (x, y) s) (x + step, y + step) (i - 1)
      in
      aux s (a, b) i;;

    test a (min_int + 1, min_int + 10000) 2 100000;;
    if info then Pervasives.print_endline "Test 1";;
    test a (max_int / 2, max_int / 2 + 10000) 2 100000;;
    if info then Pervasives.print_endline "Test 2";;
    test a (min_int + 10000, max_int / 2) 2 100000;;
    if info then Pervasives.print_endline "Test 3";;
    test a (max_int / 2, max_int - 1000000) 2 100000;;
    if info then Pervasives.print_endline "Test 4";;
    test a (max_int - 10000000, max_int - 1000000) 2 100000;;
    if info then Pervasives.print_endline "Test 5";;

    remove (min_int, max_int) a;;
    if info then Pervasives.print_endline "Starting add";;
    for i = 0 to 10000 do
      (fun _ -> ()) (add (min_int + i, max_int - i) a);
    done;;

    if info then Pervasives.print_endline "Starting remove";;
    for i = 0 to 10000 do
      (fun _ -> ()) (remove (min_int + i, max_int - i) a)
    done;;

    if info then Pervasives.print_endline "Starting split";;
    for i = 0 to 10000 do
      (fun _ -> ()) (split (min_int + i) a)
    done;;

    if info then Pervasives.print_endline "Starting below";;
    for i = 0 to 10000 do
      (fun _ -> ()) (below (min_int + i) a)
    done;;
