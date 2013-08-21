
let full = ref false 

let exclude = ref []

let err fn lineno (pos_start, pos_end) fmt =
  Printf.fprintf stdout 
    "File \"%s\", line %d, characters %d-%d:\nError:  " 
    fn lineno pos_start pos_end;
  Printf.kfprintf 
    (fun chn -> Printf.fprintf chn "\n%!")
    stdout fmt

let extract_pcre fn lineno line pat str = 
  try 
    let substr = Pcre.exec ~pat line in
    let (pos_start, pos_end) = Pcre.get_substring_ofs substr 1 in
      err fn lineno (pos_start + 1, pos_end) "%s" str
  with Not_found ->
    ()

let style_checker fn lineno line =
  let is_ml = Pcre.pmatch ~pat:"\\.mli?$" fn in
  let is_makefile = Filename.basename fn = "Makefile" in
  let eol = String.length line - 1 in
  let err pos fmt = err fn lineno pos fmt in
  let extract_pcre = extract_pcre fn lineno line in
    ignore "(*";
    if not !full then
      extract_pcre "TODO\\s*:\\s*(.*)" "new TODO.";
    if is_ml && String.length line > 80 then
      err (80, eol) "line too long (%d > 80)." (String.length line);
    extract_pcre "(\\s+)$" "line ends with spaces.";
    if is_ml then
      extract_pcre "^(\t+)" "use \\t for indentation." 
    else if is_makefile then
      extract_pcre "^\t(\t+)" "use more than one \\t for indentation."

let darcs_diff_iter chckr = 
  let chn = 
    Unix.open_process_in "darcs diff -u"
  in
  let udiff =
    UniDiff.parse (Stream.of_channel chn)
  in
    UniDiff.iter
      ~strip:1
      ~with_context:false
      `New
      (fun fn _ lineno line -> chckr fn lineno line)
      udiff;
    ignore (Unix.close_process_in chn)

module SetString = Set.Make(String)

let full_source_iter chckr = 
  let pwd = FileUtil.pwd () in

  let exclude = 
    List.rev_map
      (fun fn -> FilePath.reduce (Filename.concat pwd fn))
      !exclude 
  in

  let exclude_dirs = 
    List.fold_left
      (fun lst fn ->
         if Sys.file_exists fn && Sys.is_directory fn then
           (fn^".*") :: lst
         else
           lst)
      []
      exclude
  in

  let exclude_files = 
    List.fold_left
      (fun st fn ->
         if Sys.file_exists fn && not (Sys.is_directory fn) then
           SetString.add fn st
         else
           st)
      SetString.empty
      exclude
  in

  let prune fn = 
    let should_prune = 
      List.exists
        (fun f -> f ())
        [
          (* Exclude directories defined on the command line. *)
          (fun () ->
             List.exists
               (fun pat -> Pcre.pmatch ~pat fn)
               exclude_dirs);

          (* Exclude files defined on the command line. *)
          (fun () ->
             SetString.mem fn exclude_files);

          (* Exclude build and VCS directories. *)
          (fun () ->
             List.exists 
               (fun pat ->
                  Pcre.pmatch ~pat:(".*"^pat^".*") fn)
               [
                 "_darcs";
                 "_build";
                 ".svn";
                 ".git";
               ]);

          (* Exclude binary files. *)
          (fun () ->
             let chn = open_in_bin fn in
             let is_binary = ref false in
             let () =
               try
                   while not !is_binary do
                     is_binary := (input_char chn) = '\000'
                   done;
               with End_of_file ->
                 ()
             in
               close_in chn;
               !is_binary)
        ]
    in
      not should_prune
  in
  let iter fn =
    let chn = open_in fn in
    let lineno = ref 0 in
    let () =
      try 
        while true do 
          let line = input_line chn in
            incr lineno;
            chckr fn !lineno line
        done
      with End_of_file ->
        ()
    in
      close_in chn
  in
    FileUtil.find
      (FileUtil.And 
         (FileUtil.Is_file,
          FileUtil.Custom prune))
      (FileUtil.pwd ())
      (fun () fn -> iter fn)
      ()

let () =
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
    if !full then
      full_source_iter style_checker 
    else
      darcs_diff_iter style_checker
