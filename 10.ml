type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode xs =
    let rec loop = function
        | One a::acc, x::xs when a = x -> loop (Many (2, a)::acc, xs)
        | One a::acc, x::xs -> loop (One x::One a::acc, xs)
        | Many (c, a)::acc, x::xs when a = x -> loop (Many (c + 1, a)::acc, xs)
        | Many (c, a)::acc, x::xs -> loop (One x::Many (c, a)::acc, xs)
        | [], x::xs -> loop ([One x], xs)
        | acc, [] -> List.rev acc
    in
    loop ([], xs)

let () =
    assert (encode [] = []);
    assert (encode [1; 1; 1] = [Many (3, 1)]);
    assert (encode [1; 1; 1; 2; 3; 3] = [Many (3, 1); One 2; Many (2, 3)]);
    assert (encode [1; 2; 3; 2; 1] = [One 1; One 2; One 3; One 2; One 1]);
    print_endline "ok"
