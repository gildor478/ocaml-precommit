(* TESTS: 8 *)

(* ERROR: colon_blank_before *)
val foo : int
(* END *)

(* ERROR: colon_missing_blank_after *)
val foo:int
(* END *)

(* ERROR: none *)
f ~x:1 ()
(* END *)

(* ERROR: none *)
val f: ?x:int -> unit -> unit
(* END *)

(* ERROR: none *)
val f: x:Foo.t -> unit -> unit
(* END *)

(* ERROR: none *)
val f: x:Foo_bar.t -> unit -> unit
(* END *)

(* ERROR: none *)
val f: x:int -> y:int -> unit -> unit
(* END *)

(* ERROR: none *)
f :> g
(* END *)
