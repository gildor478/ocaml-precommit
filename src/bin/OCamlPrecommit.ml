let () =
  let full = ref false in
  let exclude = ref [] in
  let exclude_error_type = ref [] in
  let verbose = ref false in
  let () =
    Arg.parse
      [
        "--full",
        Arg.Set full,
        " Run on the full source, not only on the new content.";

        "--exclude",
        Arg.String (fun fn -> exclude := fn :: !exclude),
        "fn Exclude files and directories.";

        "--exclude-error-type",
        Arg.String (fun error_type ->
                      exclude_error_type := error_type :: !exclude_error_type),
        "error_type Exclude some error types.";

        "--verbose",
        Arg.Set verbose,
        " Be verbose.";
      ]
      (fun str -> failwith (Printf.sprintf "Don't know what to do with %S" str))
      "ocaml-precommit: check style before commiting."
  in
  let lst =
    Precommit.check
      {Precommit.
       full = !full;
       exclude = !exclude;
       exclude_error_type = !exclude_error_type;
       verbose = !verbose;
       pwd = FileUtil.pwd ()}
  in
    List.iter
      (fun err ->
         Printf.printf "%s\n%!"
           (Precommit.ocaml_err err))
      lst

