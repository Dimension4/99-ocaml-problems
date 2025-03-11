let rotate xs n =
    let n = n mod (List.length xs |> max 1) in
    List.drop n xs @ List.take n xs

let () =
    assert (rotate [] 2 = []);
    assert (rotate [1; 2; 3; 4; 5] 2 = [3; 4; 5; 1; 2]);
    assert (rotate [1; 2; 3; 4; 5] 8 = [4; 5; 1; 2; 3]);
    print_endline "ok"
