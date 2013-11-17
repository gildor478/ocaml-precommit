
module MapFilename = Map.Make(String)
module SetString = Set.Make(String)
module SetInt = Set.Make(struct type t = int let compare = (-) end)

type conf =
    {
      full: bool;
      exclude: string list;
      verbose: bool;
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

let infof conf fmt =
  (* TODO: use a generic logging library. *)
  Printf.ksprintf
    (fun str ->
       if conf.verbose then
         prerr_endline ("I: "^str))
    fmt

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

let set_eol_of_content str =
  let set = ref SetInt.empty in
    for i = 0 to (String.length str) - 1 do
      if str.[i] = '\n' then
        set := SetInt.add i !set
    done;
    !set

let line_number_of_pos set_eol pos =
  let eol_before_pos, _, _ = SetInt.split pos set_eol in
  let lineno = (SetInt.cardinal eol_before_pos) + 1 in
  let pos' =
    if lineno = 1 then
      pos
    else
      pos - (SetInt.max_elt eol_before_pos)
  in
    lineno, pos'

let err_pcre acc error set_eol line error_type pat str =
  try
    let substr_array = Pcre.exec_all ~pat line in
      Array.iter
        (fun substr ->
           let (abs_pos_start, abs_pos_end) = Pcre.get_substring_ofs substr 1 in
           let lineno, pos_start = line_number_of_pos set_eol abs_pos_start in
           (* TODO: this is now possible to have a different lineno for the end.
            *)
           let _, pos_end = line_number_of_pos set_eol abs_pos_end in
             err acc {error with lineno} pos_start pos_end error_type str)
        substr_array
  with Not_found ->
    ()

let style_checker conf fn content =
  let is_ml = Pcre.pmatch ~pat:"\\.mli?$" fn in
  let is_makefile = Filename.basename fn = "Makefile" in

  let content =
    (* Blank string to avoid matching content. *)
    let in_string = ref false in
    let is_escaped = ref false in
    let content' = String.copy content in
    (* Begin with char blanking. *)
    let content' = Pcre.replace ~pat:"'[^\\\\]'" ~templ:"' '" content' in
    let content' = Pcre.replace ~pat:"'\\\\.'" ~templ:"'  '" content' in
    (* Begin string blanking. *)
    for i = 0 to (String.length content') - 1 do
      let c = content'.[i] in
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
        content'.[i] <- ' '
    done;
    content'
  in

  let error = error_default fn 0 in
  let acc = ref [] in
  let set_eol = set_eol_of_content content in
  let err_pcre = err_pcre acc error set_eol content in
  let re_eol = "(\\n|$)" in

    (* General section. *)
    if not conf.full then begin
      ignore "(*";
      err_pcre "new_todo"
        "TODO\\s*:\\s*(.*)" "new TODO.";
    end;
    err_pcre "eol_with_space"
      ("( +)"^re_eol) "line ends with spaces.";

    if is_ml then begin
      (* .ml file *)
      err_pcre "line_too_long"
        (".{80}(.+)"^re_eol) "line too long (> 80).";
      err_pcre "tab_indent"
        "^(\t+)" "use \\t for indentation.";
      err_pcre "double_semi_colon"
        ("(;;\\s*)"^re_eol) "Use of semicolon.";
      err_pcre "missing_space"
        "([,;])[^ ;\\n]" "Missing space after ',' or ';'.";

    end else if is_makefile then begin
      (* Makefile. *)
      err_pcre "too_many_tabs"
        "^\t(\t+)" "use more than one \\t for indentation."

    end;
    List.sort (fun error1 error2 -> error2.lineno - error1.lineno) !acc

type line_range =
  | EntireFile
  | Lines of SetInt.t

let in_line_range error =
  function
    | EntireFile -> true
    | Lines allowed_lines -> SetInt.mem error.lineno allowed_lines

let string_of_line_range =
  function
    | EntireFile -> "entire file"
    | Lines allowed_lines ->
        let ranges =
          SetInt.fold
            (fun line ranges ->
               match ranges with
                 | (stop, start) :: tl when stop + 1 = line ->
                     (line, start) :: tl
                 | lst ->
                     (line, line) :: lst)
            allowed_lines
            []
        in
          String.concat ", "
            (List.rev_map
               (fun (stop, start) ->
                  if start <> stop then
                    Printf.sprintf "%d-%d" start stop
                  else
                    Printf.sprintf "%d" start)
               ranges)


let normalize_filename pwd fn =
  FilePath.reduce (Filename.concat pwd fn)

let vcs_diff_line_ranges conf =
  (* TODO: move pwd in conf. *)
  let pwd = FileUtil.pwd () in

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
  let allowed_file_with_line_ranges =
    UniDiff.fold
      ~strip:1
      ~with_context:false
      `New
      (fun map fn _ lineno _ ->
         let fn = normalize_filename pwd fn in
         let previous =
           try
             MapFilename.find fn map
           with Not_found ->
             SetInt.empty
         in
           MapFilename.add fn (SetInt.add lineno previous) map)
      MapFilename.empty
      udiff
  in
    ignore (Unix.close_process_in chn);
    MapFilename.map
      (fun allowed_lines -> Lines allowed_lines)
      allowed_file_with_line_ranges

let full_source_line_ranges conf =
  (* TODO: move pwd in conf. *)
  let pwd = FileUtil.pwd () in

  let exclude =
    List.rev_map (normalize_filename pwd) conf.exclude
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
    FileUtil.find
      (FileUtil.And
         (FileUtil.Is_file,
          FileUtil.Custom prune))
      (FileUtil.pwd ())
      (fun map fn -> MapFilename.add fn EntireFile map)
      MapFilename.empty

let get_file_content fn =
  let chn = open_in fn in
  let buff = Buffer.create (in_channel_length chn) in
    Buffer.add_channel buff chn (in_channel_length chn);
    close_in chn;
    Buffer.contents buff

let check conf =
  let all_sources = full_source_line_ranges conf in
  let restricted_sources =
    if conf.full then begin
      all_sources
    end else begin
      MapFilename.filter
        (fun fn _ ->
           (* full_source_line_ranges applies the exclusion of files, we use it
            * to restrict the vcs_diff files as well. *)
           MapFilename.mem fn all_sources)
        (vcs_diff_line_ranges conf)
    end
  in
    MapFilename.fold
      (fun fn line_range acc ->
         let () =
           infof conf "Analysing file '%s', considering error in %s"
             fn (string_of_line_range line_range)
         in
         let acc' =
           List.filter
             (fun error -> in_line_range error line_range)
             (style_checker conf fn (get_file_content fn))
         in
           List.rev_append acc' acc)
      restricted_sources []

let check_string conf fn content =
  List.rev (style_checker conf fn content)
