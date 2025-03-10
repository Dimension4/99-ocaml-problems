type 'a node =
    | One of 'a 
    | Many of 'a node list

let flatten xs =
    let rec loop acc = function
        | One x::xs -> loop (x::acc) xs
        | Many x::xs -> loop (loop acc x) xs
        | [] -> acc
    in
    loop [] xs |> List.rev

let () =
    assert (flatten [] = []);
    assert (flatten [One 1] = [1]);
    assert (flatten [Many [One 1]] = [1]);
    assert (flatten (List.init 1000 (fun x -> One x)) = List.init 1000 (fun x -> x));
    assert (flatten [One 1; Many [One 2; One 3; Many [Many [One 4; One 5]; One 6]]] = [1; 2; 3; 4; 5; 6]);
    print_endline "ok"
