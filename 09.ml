let encode xs =
    let rec loop acc cur = function
        | x::y::xs ->
            if x = y then                
                loop acc (cur + 1) (y::xs)
            else
                loop ((cur + 1, x)::acc) 0 (y::xs)
        | [x] -> loop ((cur + 1, x)::acc) 0 []
        | [] -> List.rev acc
    in
    loop [] 0 xs

let () =
    assert (encode [] = []);
    assert (encode [1; 1; 1] = [3, 1]);
    assert (encode [1; 1; 1; 2; 2; 3; 3] = [3, 1; 2, 2; 2, 3]);
    assert (encode [1; 2; 3; 2; 1] = [1, 1; 1, 2; 1, 3; 1, 2; 1, 1]);
    print_endline "ok"
