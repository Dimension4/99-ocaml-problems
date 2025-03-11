let pack xs =
    let rec loop acc cur = function
        | x::y::xs ->
            if x = y then                
                loop acc (x::cur) (y::xs)
            else
                loop ((x::cur)::acc) [] (y::xs)
        | [x] -> loop ((x::cur)::acc) [] []
        | [] -> List.rev acc
    in
    loop [] [] xs

let () =
    assert (pack [] = []);
    assert (pack [1; 1; 1] = [[1; 1; 1]]);
    assert (pack [1; 1; 1; 2; 2; 3; 3] = [[1; 1; 1]; [2; 2]; [3; 3]]);
    assert (pack [1; 2; 3; 2; 1] = [[1]; [2]; [3]; [2]; [1]]);
    print_endline "ok"
