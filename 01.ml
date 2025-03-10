let rec last = function
    | [] -> None
    | [x] -> Some x
    | x::xs -> last xs

let () =
    assert (last [] = None);
    assert (last [1] = Some 1);
    assert (last [1; 2; 3] = Some 3);
    print_endline "ok"
