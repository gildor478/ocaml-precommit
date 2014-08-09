(* TESTS: 5 *)

(* ERROR: none *)
(* END *)

(* ERROR: new_todo *)
(* TODO: blah *)
(* END *)

(* ERROR: missing_eol_eof *)
#load "foo";;(* END *)

(* ERROR: none *)
let x, y = 1, 2
(* END *)

(* ERROR: no_tuple_in_let *)
let (x, y) = 1, 2
(* END *)
