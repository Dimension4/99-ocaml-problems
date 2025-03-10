let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x, y)
    | _::xs -> last_two xs

let () =
    assert (last_two [] = None);
    assert (last_two [1] = None);
    assert (last_two [1; 2] = Some (1, 2));
    assert (last_two [1; 2; 3] = Some (2, 3));
    print_endline "ok"
