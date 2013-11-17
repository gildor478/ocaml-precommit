
open OUnit2
open Precommit


let run_check_string str =
  let conf =
    {full = false; exclude = []; verbose = false; pwd = FileUtil.pwd ()}
  in
  let ml = "foo.ml" in
  check_string conf ml str


let assert_error_type error_type str =
  match run_check_string str with
    | [error] ->
        assert_equal
          ~msg:"error type."
          ~printer:(fun s -> s)
          error_type
          error.error_type
    | errors ->
        assert_failure
          (Printf.sprintf
             "Expected 1 error, got %d errors."
             (List.length errors))


let assert_no_error str =
  assert_equal
    ~msg:"number of error."
    ~printer:string_of_int
    0
    (List.length (run_check_string str))


let () =
  run_test_tt_main
    ("OCamlPrecommit" >::
     (fun test_ctxt ->
        assert_error_type
          "double_semi_colon"
          "open Blah;;";
        assert_error_type
          "2lines_before_toplevel"
          "let f x =\n\
          \  ()\n\
          \n\
          let g x =\n\
          \  ()";
        assert_error_type
          "2lines_before_toplevel"
          "let f x =\n\
          \  ()\n\
          \n\
          \n\
          \n\
          let g x =\n\
          \  ()";

        assert_no_error "";

        assert_error_type
          "new_todo"
          "TODO: blah";

        assert_error_type
          "colon_blank_before"
          "val foo : int";
        assert_error_type
          "colon_missing_blank_after"
          "val foo:int";
        ()))
