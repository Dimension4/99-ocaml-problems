let rec duplicate = function
    | x::xs -> x::x::duplicate xs
    | [] -> []

let () =
    assert (duplicate [] = []);
    assert (duplicate [1; 2; 2] = [1; 1; 2; 2; 2; 2]);
    print_endline "ok"
