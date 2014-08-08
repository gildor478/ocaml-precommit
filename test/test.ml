
open OUnit2
open Precommit


let log_error test_ctxt error =
  logf test_ctxt `Info "Error: %s" (string_of_error error)


let run_check_string str =
  let conf =
    {
      full = false;
      exclude_files = [];
      exclude_error_type = [];
      verbose = false;
      pwd = FileUtil.pwd ()
    }
  in
  let ml = "foo.ml" in
  check_string conf ml str


let assert_error_type test_ctxt error_type str =
  match run_check_string str with
    | [error] ->
        assert_equal
          ~msg:"error type."
          ~printer:(fun s -> s)
          error_type
          error.error_type
    | errors ->
        List.iter (log_error test_ctxt) errors;
        assert_failure
          (Printf.sprintf
             "Expected 1 error, got %d errors."
             (List.length errors))


let assert_no_error test_ctxt str =
  assert_equal
    ~msg:"no errors."
    ~printer:(fun lst -> String.concat ", " (List.map string_of_error lst))
    []
    (run_check_string str)


let () =
  (* TODO: put tests in file + comment to see what kind of error to expect. *)
  run_test_tt_main
    ("OCamlPrecommit" >::
     (fun test_ctxt ->
        assert_error_type test_ctxt "double_semi_colon" "open Blah;;\n";
        assert_error_type test_ctxt "double_semi_colon" ";;\n";
        assert_error_type test_ctxt "2lines_before_toplevel"
          "let f x =\n\
          \  ()\n\
          \n\
          let g x =\n\
          \  ()\n";
        assert_error_type test_ctxt "2lines_before_toplevel"
          "let f x =\n\
          \  ()\n\
          \n\
          \n\
          \n\
          let g x =\n\
          \  ()\n";

        assert_no_error test_ctxt "\n";

        assert_error_type test_ctxt "new_todo" "TODO: blah\n";

        assert_error_type test_ctxt "colon_blank_before" "val foo : int\n";
        assert_error_type test_ctxt "colon_missing_blank_after" "val foo:int\n";
        assert_no_error test_ctxt "f ~x:1 ()\n";
        assert_no_error test_ctxt "val f: ?x:int -> unit -> unit\n";
        assert_no_error test_ctxt "val f: x:Foo.t -> unit -> unit\n";
        assert_no_error test_ctxt "val f: x:Foo_bar.t -> unit -> unit\n";
        assert_no_error test_ctxt "val f: x:int -> y:int -> unit -> unit\n";
        assert_no_error test_ctxt "f :> g\n";

        assert_error_type test_ctxt "no_tuple_in_let" "let (x, y) = 1, 2\n";
        assert_no_error test_ctxt "let x, y = 1, 2\n";

        assert_error_type test_ctxt "no_blank_begin_struct"
          "module Foo =\n\
           struct\n\
           \n\
           \  let bar = 1\n\
           end\n";

        assert_error_type test_ctxt "no_blank_begin_sig"
          "module Foo =\n\
           sig\n\
           \n\
           \   let bar = 1\n\
           end\n";
        assert_error_type test_ctxt "no_blank_end"
          "module Foo =\n\
           struct\n\
           \  let bar = 1\n\
           \n\
           end\n";

        assert_no_error test_ctxt "#load \"foo\";;\n";
        assert_error_type test_ctxt "missing_eol_eof" "#load \"foo\";;";

        assert_no_error test_ctxt "[a; b; c]\n";
        assert_error_type test_ctxt "extra_space" "[a ; b; c]\n";
        assert_error_type test_ctxt "missing_space" "[a;b; c]\n";

        assert_no_error test_ctxt "let x = a in ()\n";
        assert_no_error test_ctxt
          "let x =\n\
           \  a\n\
           in\n\
           \  ()\n";
        assert_error_type test_ctxt "let_in_format"
          "let x = a\n\
           in\
           \  ()\n";

        ()))
