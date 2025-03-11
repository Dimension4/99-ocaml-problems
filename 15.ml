let drop xs n =
    let rec loop = function
        | 1, _::xs -> loop (n, xs)
        | c, x::xs -> x::loop (c - 1, xs)
        | _, [] -> []
    in loop (n, xs)

let () =
    assert (drop [] 1 = []);
    assert (drop [1; 2; 3; 4; 5; 6] 2 = [1; 3; 5]);
    assert (drop [1] 2 = [1]);
    assert (drop [1; 2; 3; 4; 5; 6] 1 = []);
    assert (drop [1; 2] 0 = [1; 2]);
    print_endline "ok"
