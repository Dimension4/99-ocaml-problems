let rec is_palindrome xs = xs = List.rev xs

let () =
    assert (is_palindrome []);
    assert (is_palindrome [1]);
    assert (is_palindrome [1; 2; 1]);
    assert (is_palindrome [1; 2; 2; 1]);
    assert (is_palindrome [1; 2] |> not);
    print_endline "ok"
