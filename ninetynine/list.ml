exception EMPTY_LIST
exception NOT_ENOUGH_ELEMENT
exception INDEX_OUT_OF_BOUND

(* [P-1] Find the last element of a list. *)
let rec last' xs = match xs with
  | [] -> raise EMPTY_LIST
  | [x] -> x
  | _ :: xs -> find_last xs

(* [P-2] Find the last but one element of a list. *)
let rec butlast' xs = match xs with
  | [] -> raise EMPTY_LIST
  | [_] -> raise NOT_ENOUGH_ELEMENT
  | x :: _ :: [] -> x
  | _ :: x :: [_] -> x
  | _ :: xs -> find_butlast xs

(* [P-3] Find the K'th element of a list. *)
let rec at' xs k = match xs, k with
  | [], _ -> raise INDEX_OUT_OF_BOUND
  | x::_, k when k == 0 -> x
  | _::xs', k -> at' xs' (k - 1) 

(* [P-4] Find the number of elements of a list. *)
let rec length' xs = match xs with
  | [] -> 0
  | _::xs' -> 1 + (length' xs') 

(* [P-5] Reverse a list. *)
let reverse' xs = 
  let rec do_reverse' acc xxs = match acc, xxs with
    | acc, [] -> acc
    | acc, x::xxs' -> do_reverse' (x::acc) xxs' in
  do_reverse' [] xs

(* [P-6] Find out whether a list is a palindrome. *)
let palindrome' xs = xs = (reverse' xs)