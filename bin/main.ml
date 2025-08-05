open ToyCtest

(* 定义一个类型来表示我们想打印什么阶段的输出 *)
type print_stage =
  | Print_AST
  | Print_IR
  | Print_ASM
  | Print_None (* 默认只检查，不打印 *)

(* --- 主函数 --- *)
let main () =
  (* -- 1. 解析命令行参数 -- *)
  let filename = ref "" in
  let stage_to_print = ref Print_None in

  let speclist = [
    ("-p-ast", Arg.Unit (fun () -> stage_to_print := Print_AST), " 打印抽象语法树 (AST)");
    ("-p-ir", Arg.Unit (fun () -> stage_to_print := Print_IR), " 打印中间表示 (IR)");
    ("-p-asm", Arg.Unit (fun () -> stage_to_print := Print_ASM), " 打印RISC-V汇编 (ASM)");
  ] in

  let usage_msg = "用法: toycc [选项] <源文件名>" in
  (* Arg.parse 会处理 speclist 中定义的选项，并将非选项参数传给匿名函数 *)
  Arg.parse speclist (fun fname -> filename := fname) usage_msg;

  if !filename = "" then (
    prerr_endline "错误: 未指定源文件名";
    Arg.usage speclist usage_msg;
    exit 1
  );

  (* -- 2. 文件读取与词法/语法分析 -- *)
  let in_channel =
    try open_in !filename
    with Sys_error msg ->
      prerr_endline ("无法打开文件: " ^ msg);
      exit 1
  in

  let lexbuf = Lexing.from_channel in_channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !filename };

  try
    (* -- 完整的编译流程 -- *)

    (* 步骤 1: 解析得到 AST *)
    print_endline "1. 正在解析...";
    let ast = Parser.program Lexer.token lexbuf in
    close_in in_channel;

    (* (可选) 打印 AST *)
    if !stage_to_print = Print_AST then (
      print_endline "\n--- 抽象语法树 (AST) ---";
      let ast_string = Ast_printer.string_of_comp_unit ast in
      print_endline ast_string
    );

    (* 步骤 2: 语义分析 *)
    print_endline "\n2. 正在进行语义分析...";
    (* 如果分析失败, Semantic.check_program 会打印错误并退出 *)
    Semantic.check_program ast;

    (* 步骤 3: IR 生成 *)
    print_endline "\n3. 正在生成中间表示 (IR)...";
    let ir = Ast_to_ir.generate ast in

    (* 打印 IR *)
    if !stage_to_print = Print_IR then (
      print_endline "\n--- 中间表示 (IR) ---";
      let ir_string = Ir_printer.string_of_ir_program ir in
      print_endline ir_string
    );

    (* 打印 ASM *)
    if !stage_to_print = Print_ASM then (
      print_endline "\n--- RISC-V 汇编 (ASM) ---";
      let asm_string = Ir_to_asm.compile_program ir in
      print_endline asm_string
    );

    print_endline "\n编译流程完成!";
    exit 0

  with
  (* -- 异常处理 -- *)
  | Lexer.Error msg ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "\n词法错误 在 %s:%d:%d: %s\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg;
      close_in_noerr in_channel;
      exit 1

  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "\n语法错误 在 %s:%d:%d 附近 (token: '%s')\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (Lexing.lexeme lexbuf);
      close_in_noerr in_channel;
      exit 1
  
  (* 语义错误已经在 semantic.ml 内部处理并退出了，但以防万一可以加上 *)
  | Semantic.SemanticError msg ->
      Printf.eprintf "\n语义错误: %s\n" msg;
      exit 1

(* 程序入口点 *)
let () = main ()