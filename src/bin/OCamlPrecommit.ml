let () =
  let full = ref false in
  let exclude_files = ref [] in
  let exclude_error_type = ref [] in
  let verbose = ref false in
  let only_files = ref [] in
  let () =
    Arg.parse
      [
        "--full",
        Arg.Set full,
        " Run on the full source, not only on the new content.";

        "--exclude",
        Arg.String (fun fn -> exclude_files := fn :: !exclude_files),
        "fn Exclude files and directories.";

        "--exclude-error-type",
        Arg.String (fun error_type ->
                      exclude_error_type := error_type :: !exclude_error_type),
        "error_type Exclude some error types.";

        "--verbose",
        Arg.Set verbose,
        " Be verbose.";

        "--only-file",
        Arg.String (fun fn -> only_files := fn :: !only_files),
        " Analyse specific files."

      ]
      (fun str -> failwith (Printf.sprintf "Don't know what to do with %S" str))
      "ocaml-precommit: check style before commiting."
  in
  let conf =
    {Precommit.
     full = !full;
     exclude_files = !exclude_files;
     exclude_error_type = !exclude_error_type;
     verbose = !verbose;
     pwd = FileUtil.pwd ()}
  in
  let lst =
    if !only_files = [] then
      Precommit.check conf
    else
      List.flatten (List.map (Precommit.check_file conf) !only_files)
  in
    List.iter
      (fun err ->
         Printf.printf "%s\n%!"
           (Precommit.ocaml_error_of_error err))
      lst

