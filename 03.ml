let rec at i xs =
    match i, xs with
    | _ when i < 0 -> None
    | 0, x::xs -> Some x
    | _, x::xs -> at (i - 1) xs
    | _, [] -> None

let () =
    assert (at 0 [] = None);
    assert (at 0 [1] = Some 1);
    assert (at (-1) [1] = None);
    assert (at 2 [1; 2; 3] = Some 3);
    print_endline "ok"
