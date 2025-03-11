let slice xs i k = xs |> List.drop i |> List.take (k - i + 1)

let () =
    assert (slice [] 0 0 = []);
    assert (slice [1; 2; 3; 4] 0 1 = [1; 2]);
    assert (slice [1; 2; 3; 4] 2 3 = [3; 4]);
    assert (slice [1; 2; 3; 4] 0 4 = [1; 2; 3; 4]);
    print_endline "ok"
