(* TESTS: 3 *)

(* ERROR: none *)
[a; b; c]
(* END *)

(* ERROR: extra_space *)
[a ; b; c]
(* END *)

(* ERROR: missing_space *)
[a;b; c]
(* END *)
