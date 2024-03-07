let () = print_endline "Hello, World!"

let () =
  let argc = Array.length Sys.argv in
  if argc != 2 then (
    Format.printf "Usage: ./mic [filename]\n";
    exit (-1))
  else
    let fname = Sys.argv.(1) in
    let inchan = open_in fname in
    let filebuf = Lexing.from_channel inchan in
    ignore (Mic.Parser.translation_unit Mic.Lexer.token filebuf);
    print_endline
      (Mic.Typing.show_typed_programi
         (List.mapi
            (fun i x -> (i, x))
            (Mic.Typing.type_program (List.rev !Mic.Env.program))));
        print_endline
      (Mic.Syntax.show_programi
         (List.mapi
            (fun i x -> (i, x))
            ( (List.rev !Mic.Env.program))));
    print_endline
      (Mic.Cgen.gen_decl "x"
         (Mic.Syntax.TConstPtr (Mic.Syntax.TDeclSpec [ Mic.Syntax.TsInt ])));
    print_endline
      (Mic.Cgen.gen_decl "func"
         (Mic.Syntax.TConstPtr
            (Mic.Syntax.TFun
               ( Mic.Syntax.TDeclSpec [ Mic.Syntax.TsVoid ],
                 [ ("a", Mic.Syntax.TDeclSpec [ Mic.Syntax.TsInt ]) ] ))));
    print_endline
      (Mic.Cgen.gen_decl "var"
         (Mic.Syntax.TArr
            ( Mic.Syntax.TPtr (Mic.Syntax.TDeclSpec [ Mic.Syntax.TsChar ]),
              Mic.Syntax.EBinary
                ( Mic.Syntax.Add,
                  Mic.Syntax.EConst (Mic.Syntax.VInt "4"),
                  Mic.Syntax.EConst (Mic.Syntax.VInt "5") ) )));
    print_endline
      (Mic.Cgen.gen_decl "x" (Mic.Syntax.TDeclSpec [ TsStructDef 0 ]));
    print_endline
      (Mic.Cgen.gen_decl "p"
         (Mic.Syntax.TPtr
          (Mic.Syntax.TArr (
             (Mic.Syntax.TPtr
                (Mic.Syntax.TFun ((Mic.Syntax.TDeclSpec [Mic.Syntax.TsVoid]),
                   [("a", (Mic.Syntax.TDeclSpec [Mic.Syntax.TsInt]));
                     ("b", (Mic.Syntax.TDeclSpec [Mic.Syntax.TsInt]))]
                   ))),
             (Mic.Syntax.EConst (Mic.Syntax.VInt "10"))))) );
    print_endline
      (Mic.Cgen.gen_decl "signal"
       (Mic.Syntax.TFun (
          (Mic.Syntax.TPtr
             (Mic.Syntax.TFun ((Mic.Syntax.TDeclSpec [Mic.Syntax.TsVoid]),
                [("b", (Mic.Syntax.TDeclSpec [Mic.Syntax.TsInt]))]))),
          [("sig", (Mic.Syntax.TDeclSpec [Mic.Syntax.TsInt]));
            ("func",
             (Mic.Syntax.TPtr
                (Mic.Syntax.TFun ((Mic.Syntax.TDeclSpec [Mic.Syntax.TsVoid]),
                   [("a", (Mic.Syntax.TDeclSpec [Mic.Syntax.TsInt]))]))))
            ]
          )))