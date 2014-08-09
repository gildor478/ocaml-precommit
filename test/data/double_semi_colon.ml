(* TESTS: 3 *)

(* ERROR: double_semi_colon *)
open Blah;;
(* END *)

(* ERROR: double_semi_colon *)
;;
(* END *)

(* ERROR: none *)
#load "foo";;
(* END *)
