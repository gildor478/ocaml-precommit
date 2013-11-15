
type conf =
    {
      full: bool;
      exclude: string list;
    }

type error =
    {
      filename: string;
      lineno: int;
      pos_start: int;
      pos_end: int;
      message: string;
      error_type: string;
    }

let ocaml_err error =
  Printf.sprintf
    "File \"%s\", line %d, characters %d-%d:\nError: %s"
    error.filename error.lineno error.pos_start error.pos_end error.message

let error_default fn lineno =
  {
    filename = fn;
    lineno = lineno;
    pos_start = 0;
    pos_end = 0;
    message = "<none>";
    error_type = "none";
  }

let err acc error pos_start pos_end error_type msg =
  acc := {error with pos_start; pos_end; message = msg; error_type} :: !acc

let errf acc error pos_start pos_end error_type fmt =
  Printf.ksprintf (err acc error pos_start pos_end error_type) fmt

let err_pcre acc error line error_type pat str =
  try
    let substr = Pcre.exec ~pat line in
    let (pos_start, pos_end) = Pcre.get_substring_ofs substr 1 in
      err acc error (pos_start + 1) pos_end error_type str
  with Not_found ->
    ()

let style_checker conf acc fn lineno line =
  let is_ml = Pcre.pmatch ~pat:"\\.mli?$" fn in
  let is_makefile = Filename.basename fn = "Makefile" in

  (* TODO: We need a global approach per file to match more things. *)
  let line =
    (* Blank string to avoid matching content. *)
    let in_string = ref false in
    let is_escaped = ref false in
    let line' = String.copy line in
    (* Begin with char blanking. *)
    let line' = Pcre.replace ~pat:"'[^\\\\]'" ~templ:"' '" line' in
    let line' = Pcre.replace ~pat:"'\\\\.'" ~templ:"'  '" line' in
    (* Begin string blanking. *)
    for i = 0 to (String.length line') - 1 do
      let c = line'.[i] in
      let erase =
        if !is_escaped then begin
          is_escaped := false;
          true
        end else if !in_string then begin
          match c with
            | '"' when not !is_escaped ->
                in_string := false;
                is_escaped := false;
                false
            | '\\' ->
                is_escaped := true;
                true
            | _ ->
                true
        end else begin
          match c with
            | '"' -> in_string := true; false
            | _ -> false
        end
      in
      if erase then
        line'.[i] <- ' '
    done;
    line'
  in

  let linelen = String.length line in
  let eol_pos = linelen - 1 in

  let error = error_default fn lineno in
  let errf pos_start pos_end error_type fmt =
    errf acc error pos_start pos_end error_type fmt
  in
  let err_pcre = err_pcre acc error line in

    (* General section. *)
    if not conf.full then begin
      ignore "(*";
      err_pcre "new_todo"
        "TODO\\s*:\\s*(.*)" "new TODO.";
    end;
    err_pcre "eol_with_space"
      "(\\s+)$" "line ends with spaces.";

    if is_ml then begin
      (* .ml file *)
      if linelen > 80 then
        errf 80 eol_pos "line_too_long"
          "line too long (%d > 80)." linelen;
      err_pcre "tab_indent"
        "^(\t+)" "use \\t for indentation.";
      err_pcre "double_semi_colon"
        "(;;)$" "Use of semicolon.";
      err_pcre "missing_space"
        "([,;])[^ $;]" "Missing space after ',' or ';'.";

    end else if is_makefile then begin
      (* Makefile. *)
      err_pcre "too_many_tabs"
        "^\t(\t+)" "use more than one \\t for indentation."

    end

let vcs_diff_iter conf chckr =
  let chn =
    if Sys.file_exists "_darcs" then
      Unix.open_process_in "darcs diff -u"
    else if Sys.file_exists ".git" then
      Unix.open_process_in "git diff -u"
    else
      failwith "Cannot identify VCS."
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

let full_source_iter conf chckr =
  let pwd = FileUtil.pwd () in

  let exclude =
    List.rev_map
      (fun fn -> FilePath.reduce (Filename.concat pwd fn))
      conf.exclude
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

let check conf =
  let acc = ref [] in
  let () =
    if conf.full then
      full_source_iter conf (style_checker conf acc)
    else
      vcs_diff_iter conf (style_checker conf acc)
  in
    List.rev !acc

let check_string conf fn str =
  let acc = ref [] in
  let () = style_checker conf acc fn 1 str in
    List.rev !acc
