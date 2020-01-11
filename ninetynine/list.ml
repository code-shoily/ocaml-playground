exception EMPTY_LIST
exception NOT_ENOUGH_ELEMENT
exception INDEX_OUT_OF_BOUND

(* [P-1] Find the last element of a list. *)
let rec last = function
  | [] -> raise EMPTY_LIST
  | [x] -> x
  | _ :: xs -> last xs

(* [P-2] Find the last but one element of a list. *)
let rec butlast = function
  | [] -> raise EMPTY_LIST
  | [_] -> raise NOT_ENOUGH_ELEMENT
  | [x; _] -> x
  | _ :: x :: [_] -> x
  | _ :: xs -> butlast xs

(* [P-3] Find the K'th element of a list. *)
let rec at xs k = match xs, k with
  | [], _ -> raise INDEX_OUT_OF_BOUND
  | x::_, k when k == 0 -> x
  | _::xs', k -> at xs' (k - 1) 

(* [P-4] Find the number of elements of a list. *)
let rec len = function
  | [] -> 0
  | _::xs -> 1 + (len xs) 

(* [P-5] Reverse a list. *)
let rev xs = 
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (x::acc) xs in
  aux [] xs

(* [P-6] Find out whether a list is a palindrome. *)
let is_palin xs = xs = (rev xs)
