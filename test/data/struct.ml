(* TESTS: 3 *)

(* ERROR: no_blank_begin_struct *)
module Foo =
struct

  let bar = 1
end
(* END *)

(* ERROR: no_blank_begin_sig *)
module Foo =
sig

   let bar = 1
end
(* END *)

(* ERROR: no_blank_end *)
module Foo =
struct
  let bar = 1

end
(* END *)
