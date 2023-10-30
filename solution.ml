(* Solution 01 *)
(* Tail of a List *)
let rec last = function
        | [] -> None
        | [x] -> Some x
        | _ :: tail -> last tail

(* Solution 02 *)
(* Last Two Elements of a List *)
let rec last_two = function
        | [] -> None
        | [_] -> None
        | [a; b] -> Some (a, b)
        | _ :: tail -> last_two tail

(* Solution 03 *)
(* N'th Element of List *)
let rec at k = function
        | [] -> None
        | head :: tail -> if k = 0 then Some head else at (k - 1) tail

(* Solution 04 *)
(* Length of a List *)
let length (li: 'a list) =
        let rec length_inner (acc: int) = function
                | [] -> acc
                | _ :: tail -> length_inner (acc + 1) tail
        in length_inner 0 li

(* Solution 05 *)
(* Reverse a List *)
let rev li =
        let rec rev' result = function
                | [] -> result
                | head :: tail -> rev' (head :: result) tail
        in rev' [] li
