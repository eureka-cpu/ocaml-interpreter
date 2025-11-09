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

(* Reverse a list.
  OCaml standard library has `List.rev` but we ask that you reimplement it. *)
let rec aux acc = function
  | [] -> acc
  | head :: tail -> aux (head :: acc) tail
and rev_tr list = aux [] list in
test "rev" (fun () -> assert (rev_tr [ "a"; "b"; "c"; "d" ] = [ "d"; "c"; "b"; "a" ]))
;;

let is_palendrome list = List.rev list = list in
test "is_palendrome" (fun () -> assert (is_palendrome [ "a"; "b"; "b"; "a" ]));
test "is_not_palendrome" (fun () -> assert (not (is_palendrome [ "a"; "b"; "c"; "d" ])))

(* If you need so, refresh your memory about [run-length encoding](http://en.wikipedia.org/wiki/Run-length_encoding)
  `inc` is the incrementor for the number of consecutive occurrances of the same data, and `acc` is the accumulator
  which is where we store the pairs of data where the first element of the tuple is `inc` and the second is the data *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let new_rle len elem = if len = 1 then One elem else Many (len, elem);;

let rec rle_encode_aux inc acc = function
  | [] -> [] (* The list is empty *)
  | [ head ] ->
    new_rle (inc + 1) head :: acc (* The list contains only one of this element *)
  | head :: (next_head :: _ as tail) ->
    (* compare the current head to the next to increase count *)
    if head = next_head
    then rle_encode_aux (inc + 1) acc tail
    else rle_encode_aux 0 (new_rle (inc + 1) head :: acc) tail
and rle_encode_list_tr list = List.rev (rle_encode_aux 0 [] list) in
test "rle_encode_list" (fun () ->
  assert (
    rle_encode_list_tr
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]))
;;

print_endline "\nAll tests completed."
