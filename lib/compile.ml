let tmp_file1 fname =
  let fnlen = String.length fname in
  let suffix = if 3 < fnlen then String.sub fname (fnlen - 2) 2 else "" in
  if suffix = ".c" then Printf.sprintf "%s.c" (String.sub fname 0 (fnlen - 2))
  else if suffix = "mi" then
    Printf.sprintf "%s.c" (String.sub fname 0 (fnlen - 3))
  else Printf.sprintf "%s.c" fname

let tmp_file2 fname =
  let fnlen = String.length fname in
  let suffix = if 3 < fnlen then String.sub fname (fnlen - 2) 2 else "" in
  if suffix = ".c" then Printf.sprintf "%s.i" (String.sub fname 0 (fnlen - 2))
  else Printf.sprintf "%s.i" fname

let out_file fname =
  let fnlen = String.length fname in
  let suffix = if 2 < fnlen then String.sub fname (fnlen - 2) 2 else "" in
  if suffix = ".c" then Printf.sprintf "%s.o" (String.sub fname 0 (fnlen - 2))
  else Printf.sprintf "%s.o" fname

let show_pos fname filebuf =
  let pos = filebuf.Lexing.lex_start_p in
  Printf.eprintf "File \"%s\", line %d, character %d:\n" fname
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let show_error fmt =
  let go str =
    Printf.eprintf "error: %s\n" str;
    exit 1
  in
  Printf.ksprintf go fmt

let compile fname =
  ignore (Sys.command ("cp " ^ fname ^ " " ^ tmp_file1 fname));
  let tmp = tmp_file1 fname in
  ignore (Sys.command ("gcc -E " ^ tmp ^ " > " ^ tmp_file2 tmp));
  let inchan = open_in (tmp_file2 tmp) in
  let filebuf = Lexing.from_channel inchan in
  try
    ignore (Parser.translation_unit Lexer.token filebuf);
    (*print_endline
      (Syntax.show_programi
         (List.mapi (fun i x -> (i, x)) (List.rev !Env.program)));*)
    (*print_endline
      (Typing.show_typed_programi
         (List.mapi
            (fun i x -> (i, x))
            (Typing.type_program (List.rev !Env.program))));*)
    let program = Cgen.gen_program (List.rev !Env.program) in
    let outchan = open_out tmp in
    Printf.fprintf outchan "%s" program;
    close_out outchan;
    let tmp = tmp_file1 tmp in
    ignore (Sys.command ("gcc " ^ tmp ^ " -o " ^ out_file tmp ^ " -lmimalloc"));
    out_file tmp
  with
  | Failure msg -> show_error "%s" msg
  | _ ->
      show_pos fname filebuf;
      show_error "parser: syntax error near '%s'" (Lexing.lexeme filebuf)
