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


let test_of_file bn =
  bn >::
  (fun test_ctxt ->
     let fn = in_testdata_dir test_ctxt [bn] in
     let chn = open_in fn in
     let content = String.create (in_channel_length chn) in
     let () =
       really_input chn content 0 (String.length content);
       close_in chn;
       logf test_ctxt `Info "File content: \n%s" content
     in
     (* Extract the number of expected tests. *)
     let number =
       let substr =
         try
           Pcre.exec ~pat:"\\(\\* TESTS: (\\d+) \\*\\)" content
         with Not_found ->
           failwith "Unable to find the number of test."
       in
         int_of_string (Pcre.get_substring substr 1)
     in
     let arr =
       ignore "(*(*";
       Pcre.exec_all
         ~pat:"\\(\\* ERROR: (.*) \\*\\)((\\n|.)*?)\\(\\* END \\*\\)"
         content
     in
       if Array.length arr <> number then
         failwith
           (Printf.sprintf "Expecting to find %d tests, but found %d."
              number (Array.length arr));
       Array.iter
         (fun substr ->
            let error = Pcre.get_substring substr 1 in
            let text = Pcre.get_substring substr 2 in
              logf test_ctxt `Info "Expecting error: %s" error;
              logf test_ctxt `Info "Text: \n%s" text;
              if error = "none" then
                assert_no_error test_ctxt text
              else
                assert_error_type test_ctxt error text)
         arr)


let test_files =
  [
    "double_semi_colon.ml";
    "space.ml";
    "let_in_format.ml";
    "2lines_before_toplevel.ml";
    "colon.ml";
    "misc.ml";
    "struct.ml";
    "ifelse.ml";
  ]

module StringDiff =
  OUnitDiff.SetMake
    (struct
       type t = string
       let compare = String.compare
       let pp_printer = Format.pp_print_string
       let pp_print_sep = OUnitDiff.pp_comma_separator
     end)

let () =
  run_test_tt_main
    ("OCamlPrecommit" >:::
     ["CheckTestFiles" >::
      (fun test_ctxt ->
         (* Check that we have all tests. *)
         let got =
           List.filter
             (fun str -> String.length str > 0 && str.[0] <> '.')
             (Array.to_list (Sys.readdir (in_testdata_dir test_ctxt [])))
         in
           StringDiff.assert_equal
             (StringDiff.of_list test_files)
             (StringDiff.of_list got));

      "File" >:::
      (List.map test_of_file test_files)])
