(* [P-1] Find the last element of a list. *)
let rec last = function
  | [] -> failwith "Empty list"
  | [x] -> x
  | _ :: xs -> last xs

(* [P-2] Find the last but one element of a list. *)
let rec butlast = function
  | [] | [_] -> failwith "Not enough elements"
  | [x; _] -> x
  | _ :: xs -> butlast xs

(* [P-3] Find the K'th element of a list. *)
let rec at xs k = match xs, k with
  | [], _ -> failwith "Index out of bound"
  | x :: _, k when k == 0 -> x
  | _ :: xs', k -> at xs' (k - 1) 

(* [P-4] Find the number of elements of a list. *)
let rec len = function
  | [] -> 0
  | _ :: xs -> 1 + (len xs) 

(* [P-5] Reverse a list. *)
let rev xs = 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs in
  aux [] xs

(* [P-6] Find out whether a list is a palindrome. *)
let is_palin xs = xs = (rev xs)

(* [P-8] Eliminate consecutive duplicates of list elements. *)
let rec uniq = function
  | [] -> []
  | x :: y :: xs when x = y -> uniq (x :: xs)
  | x :: xs -> x :: (uniq xs)
