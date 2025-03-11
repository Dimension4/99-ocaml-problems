let replicate xs n =
    let rec loop acc = function
        | 0, _::xs -> loop acc (n, xs)
        | c, x::xs -> loop (x::acc) (c - 1, x::xs)
        | _, [] -> acc
    in loop [] (n, List.rev xs)
    
let () =
    assert (replicate [] 0 = []);
    assert (replicate [] 1 = []);
    assert (replicate [1] 0 = []);
    assert (replicate [1] 1 = [1]);
    assert (replicate [1] 3 = [1; 1; 1]);
    assert (replicate [1; 2; 1] 2 = [1; 1; 2; 2; 1; 1]);
    print_endline "ok"
