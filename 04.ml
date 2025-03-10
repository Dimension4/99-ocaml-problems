let rec rev = function
    | [] -> []
    | x::xs -> rev xs @ [x]

let () =
    assert (rev [] = []);
    assert (rev [1] = [1]);
    assert (rev [1; 2; 3] = [3; 2; 1]);
    let l = List.init 10_000 (fun x -> x) in
    assert (rev l = List.rev l);
    print_endline "ok"
