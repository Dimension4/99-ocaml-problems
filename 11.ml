type 'a rle =
    | One of 'a
    | Many of int * 'a

let decode xs =
    let rec loop acc = function
        | One x::xs -> loop (x::acc) xs
        | Many (1, x)::xs -> loop (x::acc) xs
        | Many (c, x)::xs -> loop (x::acc) (Many (c - 1, x)::xs)
        | [] -> List.rev acc
    in loop [] xs

let () =
    assert (decode [] = []);
    assert (decode [Many (3, 1)] = [1; 1; 1]);
    assert (decode [Many (3, 1); One 2; Many (2, 3)] = [1; 1; 1; 2; 3; 3]);
    assert (decode [One 1; One 2; One 3; One 2; One 1] = [1; 2; 3; 2; 1]);
    print_endline "ok"
