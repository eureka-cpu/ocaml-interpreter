(** Provide a name and an assertion and print its outcome *)
let test name f =
  let pass s = "\027[32m" ^ s ^ "\027[0m"
  and fail s = "\027[31m" ^ s ^ "\027[0m" in
  try
    f ();
    print_string (pass "✓ " ^ name ^ " -- " ^ pass "pass\n")
  with
  | e -> print_endline (fail "✗ " ^ name ^ " -- " ^ fail ("fail: " ^ Printexc.to_string e))
;;

print_endline "";;

(* Write a function `last : 'a list -> 'a option` that returns the last element of a list *)
let rec tail_of_a_list = function
  | [] -> None
  | [ tail ] -> Some tail
  | _ :: remaining -> tail_of_a_list remaining
in
test "tail_of_a_list_some" (fun () ->
  assert (tail_of_a_list [ "a"; "b"; "c"; "d" ] = Some "d"));
test "tail_of_a_list_none" (fun () -> assert (tail_of_a_list [] = None))
;;

(* Find the last two (last and penultimate) elements of a list *)
let rec last_two_of_a_list = function
  | [] | _ :: [] -> None
  | [ penultimate; last ] -> Some (penultimate, last)
  | _head :: remaining -> last_two_of_a_list remaining
in
test "last_two_of_a_list_some" (fun () ->
  assert (last_two_of_a_list [ "a"; "b"; "c"; "d" ] = Some ("c", "d")));
test "last_two_of_a_list_none" (fun () -> assert (last_two_of_a_list [ "a" ] = None))
;;

(* Find the N'th element of a list *)
let rec get_elem_at index = function
  | [] -> None
  | elem :: remaining ->
    if index = 0 then Some elem else get_elem_at (index - 1) remaining
in
test "get_elem_at_some" (fun () ->
  assert (get_elem_at 2 [ "a"; "b"; "c"; "d" ] = Some "c"));
test "get_elem_at_none" (fun () -> assert (get_elem_at 2 [ "a"; "b" ] = None))
;;

(* Find the number of elements of a list.
  OCaml standard library has `List.length` but we ask that you reimplement it. Bonus for a tail recursive solution. *)
let rec count_aux acc = function
  | [] -> acc
  | _ :: remaining -> count_aux (acc + 1) remaining
and length_tr list = count_aux 0 list in
test "length_tr" (fun () -> assert (length_tr [ "a"; "b"; "c" ] = 3));
test "length_tr_empty" (fun () -> assert (length_tr [] = 0))
;;

print_endline "\nAll tests completed."
