open Printf

(* https://ocaml.org/docs/data-types *)
(* https://baturin.org/docs/ocaml-faq/ *)

let print_list l =
  print_char '(';
  List.iter (printf "%s ") l;
  print_char ')'

(* Duplicate the Elements of a List *)
let duplicate l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> (aux [@tailcall]) (h :: h :: acc) t
  in
  List.rev (aux [] l)

let () = print_list (duplicate [ "a"; "b"; "c"; "c"; "d" ])
let () = print_newline ()

(* Replicate the Elements of a List a Given Number of Times *)
let replicate l n =
  let rec expand acc c = function
    | 0 -> acc
    | n -> (expand [@tailcall]) (c :: acc) c (n - 1)
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> (aux [@tailcall]) (expand [] h n @ acc) t
  in
  aux [] (List.rev l)

let () = print_list (replicate [ "a"; "b"; "c" ] 4)
let () = print_newline ()

let replicate2 l n =
  let rec expand i acc c =
    match i with 0 -> acc | n -> (expand [@tailcall]) (n - 1) (c :: acc) c
  in
  List.fold_left (expand n) [] (List.rev l)

let () = print_list (replicate2 [ "a"; "b"; "c" ] 4)
let () = print_newline ()

(* Drop Every N'th Element From a List *)
let drop l n =
  let rec aux acc i = function
    | [] -> acc
    | h :: t ->
        if i = 0 then
          (aux [@tailcall]) acc (n - 1) t
        else
          (aux [@tailcall]) (h :: acc) (i - 1) t
  in
  List.rev (aux [] (n - 1) l)

let () =
  print_list (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)

let () = print_newline ()

let split l n =
  let rec aux acc n = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if n = 0 then
          (List.rev acc, l)
        else
          aux (h :: acc) (n - 1) t
  in
  aux [] n l

let a, b = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
let () = print_list a
let _ = print_list b
let () = print_newline ()
let a, b = split [ "a"; "b"; "c"; "d" ] 5
let () = print_list a
let () = print_list b
let () = print_newline ()

let slice l i k =
  let rec take acc n = function
    | [] -> acc
    | h :: t ->
        if n = 0 then
          acc
        else
          (take [@tailcall]) (h :: acc) (n - 1) t
  in
  let rec drop n = function
    | [] -> []
    | _ :: t as l ->
        if n = 0 then
          l
        else
          (drop [@tailcall]) (n - 1) t
  in
  List.rev (take [] (k - i + 1) (drop i l))

let () =
  print_list (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6)

let () = print_newline ()

let rotate l n =
  let len = List.length l in
  let n =
    if len = 0 then
      0
    else
      ((n mod len) + len) mod len
  in
  if n = 0 then
    l
  else
    let a, b = split l n in
    b @ a

let remove_at n list =
  let rec aux acc n = function
    | [] -> List.rev acc
    | h :: t ->
        if n = 0 then
          List.rev acc @ t
        else
          aux (h :: acc) (n - 1) t
  in
  aux [] n list

let () = print_list (remove_at 1 [ "a"; "b"; "c"; "d" ])
let () = print_newline ()

let insert_at el at list =
  let rec aux acc i = function
    | [] -> List.rev (el :: acc)
    | h :: t as l ->
        if i = 0 then
          List.rev (el :: acc) @ l
        else
          aux (h :: acc) (i - 1) t
  in
  aux [] at list

let () = print_list (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ])
let () = print_newline ()

(*  Create a List Containing All Integers Within a Given Range  *)
let range a b =
  let rec aux acc i =
    if i > b then
      acc
    else
      aux (i :: acc) (i + 1)
  in
  List.rev (aux [] a)

let () = print_list (List.map string_of_int (range 4 9))
let () = print_newline ()
let () = print_list (List.map string_of_int (range 10 9))
let () = print_newline ()

let rand_select list count =
  let extract list i =
    let rec aux acc i = function
      | [] -> raise Not_found
      | h :: t ->
          if i = 0 then
            (h, List.rev acc @ t)
          else
            aux (h :: acc) (i - 1) t
    in
    aux [] i list
  in
  let rec aux acc i list =
    if i = 0 then
      acc
    else
      match list with
      | [] -> acc
      | l ->
          let el, remaining = extract l (Random.bits () mod List.length l) in
          aux (el :: acc) (i - 1) remaining
  in
  aux [] count list

let () = print_list (rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
let () = print_newline ()

let lotto_select n m = rand_select (range 1 m) n
let () = print_list (List.map string_of_int (lotto_select 6 49))
let () = print_newline ()

let permutation list = rand_select list (List.length list)
let () = print_list (permutation [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ])
let () = print_newline ()

let rec list_sort cmp list = 
  let rec insert cmp el = function 
    | [] -> [el]
    | h::t as l -> if cmp el h <= 0 then el::l else h::(insert cmp el t)
in
match list with
| [] -> []
| h :: t -> insert cmp h (list_sort cmp t)

let length_sort list = List.map snd (list_sort (fun a b -> fst a - fst b) (List.map (fun l -> (List.length l, l)) list))

let () = print_char '('
let l = length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]
let _ = List.map print_list l
let () = print_char ')'
let () = print_newline ()
