let () =
  let full = ref false in
  let exclude = ref [] in
  let () = 
    Arg.parse
      [
        "--full",
        Arg.Set full,
        " Run on the full source, not only on the new content.";

        "--exclude",
        Arg.String (fun fn -> exclude := fn :: !exclude),
        "fn Exclude files and directories.";
      ]
      (fun str -> failwith (Printf.sprintf "Don't know what to do with %S" str))
      "ocaml-precommit: check style before commiting."
  in
  let lst =
    Precommit.check
      {Precommit.full = !full; Precommit.exclude = !exclude}
  in
    List.iter
      (fun err ->
         Printf.printf "%s\n%!"
           (Precommit.ocaml_err err))
      lst

