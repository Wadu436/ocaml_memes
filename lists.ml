open Printf

let print_list l = 
  print_char '(';
  List.iter (printf "%s ") l;
  print_char ')';
;;

(* Duplicate the Elements of a List *)
let duplicate l =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> (aux [@tailcall]) (h :: h :: acc) t
  in
  List.rev (aux [] l);;

print_list (duplicate ["a"; "b"; "c"; "c"; "d"]);;
print_newline ();;

(* Replicate the Elements of a List a Given Number of Times *)
let replicate l n = 
  let rec expand acc c = function
  | 0 -> acc
  | n -> (expand [@tailcall]) (c::acc) c (n-1)
in
let rec aux acc = function
| [] -> acc
| h::t -> (aux [@tailcall]) ((expand [] h n) @ acc) t
in
aux [] (List.rev l);;

print_list (replicate ["a"; "b"; "c"] 4);;
print_newline ();;

let replicate2 l n =
  let rec expand i acc c  = match i with
  | 0 -> acc
  | n -> (expand [@tailcall]) (n-1) (c::acc) c
in
List.fold_left (expand n) [] (List.rev l);;

print_list (replicate2 ["a"; "b"; "c"] 4);;
print_newline ();;

(* Drop Every N'th Element From a List *)
let drop l n =
let rec aux acc i = function
  | [] -> acc
  | h :: t -> if i = 0 then (aux [@tailcall]) acc (n-1) t else (aux [@tailcall]) (h::acc) (i-1) t 
in
List.rev (aux [] (n-1) l);;

print_list (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3);;
print_newline ();;

let split l n = 
  let rec aux acc n = function
  | [] -> (List.rev acc, [])
  | h::t -> if n = 0 then (List.rev acc, l) else aux (h::acc) (n-1) t
in aux [] n l;;

let a, b = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 in
print_list a; print_list b;;
print_newline ();;

let a, b = split ["a"; "b"; "c"; "d"] 5 in
print_list a; print_list b;;
print_newline ();;

let slice l i k =
  let rec take acc n =
    function
    | [] -> acc
    | h::t -> if n = 0 then acc else (take [@tailcall]) (h::acc) (n-1) t
  in
  let rec drop n =
    function
    | [] -> []
    | _::t as l -> if n = 0 then l else (drop [@tailcall]) (n-1) t 
  in
  List.rev (take [] (k-i+1) (drop i l));;

print_list (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6);;
print_newline ();;

let rotate l n = 
  let len = List.length l in
  let n = if len = 0 then 0 else (n mod len + len) mod len in
  if n = 0 then l
  else let a, b = split l n in b@a;;

let remove_at n list = 
  let rec aux acc n = 
    function
    | [] -> List.rev acc
    | h::t -> if n = 0 then (List.rev acc) @ t else aux (h::acc) (n-1) t
  in
  aux [] n list;;

print_list (remove_at 1 ["a"; "b"; "c"; "d"]);;
print_newline ();;

let insert_at el at list = 
let rec aux acc i = 
  function 
  | [] -> (List.rev (el::acc)) 
  | h::t as l -> if i = 0 then (List.rev (el::acc)) @ l else aux (h::acc) (i-1) t
in
  aux [] at list;;

print_list (insert_at "alfa" 1 ["a"; "b"; "c"; "d"]);;
print_newline ();;