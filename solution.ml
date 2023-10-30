(* Solution 01 *)
(* Tail of a List *)
let rec last (li: 'a list) =
        match li with
        | [] -> None
        | [x] -> Some x
        | _ :: tail -> last tail

(* Solution 02 *)
(* Last Two Elements of a List *)
let rec last_two (li: 'a list) =
        match li with
        | [] -> None
        | [_] -> None
        | [a; b] -> Some (a, b)
        | _ :: tail -> last_two tail
