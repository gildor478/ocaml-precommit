
open OUnit2
open Precommit

let assert_error_type error_type str =
  let conf =
    {full = false; exclude = []; verbose = false; pwd = FileUtil.pwd ()}
  in
  let ml = "foo.ml" in
  let errors = check_string conf ml str in
    match errors with
      | [error] ->
          assert_equal
            ~msg:"error type"
            ~printer:(fun s -> s)
            error_type
            error.error_type
      | _ ->
          assert_failure
            (Printf.sprintf
               "Expected 1 error, got %d errors."
               (List.length errors))

let () =
  run_test_tt_main
    ("OCamlPrecommit" >::
     (fun test_ctxt ->
        assert_error_type
          "double_semi_colon"
          "open Blah;;"))
