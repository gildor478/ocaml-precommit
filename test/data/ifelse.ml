(* TESTS: 6 *)

(* ERROR: none *)
if cond1 then e1 else
if cond2 then e2 else
if cond3 then e3 else
  e4
(* END *)

(* ERROR: none *)
if cond then begin
  e1
end else
if cond2 then begin
  e2
end else
if cond3 then
  e3
else
  e4
(* END *)

(* ERROR: none *)
if cond then e1 else e2
(* END *)

(* ERROR: none *)
if cond then begin
  e1
end else begin
  e2
end
(* END *)

(* ERROR: extra_parentheses *)
if cond then
  (
    e1
  )
(* END *)

(* ERROR: extra_parentheses *)
if cond then e1 else
  (
    e2
  )
(* END *)
