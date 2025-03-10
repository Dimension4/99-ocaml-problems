let rec compress = function
    | x::y::xs ->
        if x = y
        then compress (y::xs)
        else x::compress (y::xs)
    | x -> x

let () =
    assert (compress [] = []);
    assert (compress [1; 2; 3; 4] = [1; 2; 3; 4]);
    assert (compress [1; 1; 2; 2] = [1; 2]);
    assert (compress [1; 1; 2; 2; 2; 1] = [1; 2; 1]);
    print_endline "ok"
