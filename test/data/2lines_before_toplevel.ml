(* TESTS: 3 *)

(* ERROR: none *)
let f x =
  ()


let g x =
  ()
(* END *)

(* ERROR: 2lines_before_toplevel *)
let f x =
  ()

let g x =
  ()
(* END *)


(* ERROR: 2lines_before_toplevel *)
let f x =
  ()



let g x =
  ()
(* END *)
