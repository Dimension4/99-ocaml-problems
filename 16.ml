let split xs n =
    let rec loop acc = function
        | c, xs when c = n -> List.rev acc, xs
        | c, x::xs -> loop (x::acc) (c + 1, xs)
        | _, [] -> List.rev acc, []
    in loop [] (0, xs)

let () =
    assert (split [] 0 = ([], []));
    assert (split [1; 2] 0 = ([], [1; 2]));
    assert (split [1; 2] 1 = ([1], [2]));
    assert (split [1; 2] 2 = ([1; 2], []));
    assert (split [1; 2; 3; 4; 5] 2 = ([1; 2], [3; 4; 5]));
    print_endline "ok"
