(* TESTS: 3 *)

(* ERROR: none *)
let x = a in ()
(* END *)

(* ERROR: none *)
let x =
  a
in
  ()
(* END *)

(* ERROR: let_in_format *)
let x = a
in
  ()
(* END *)
