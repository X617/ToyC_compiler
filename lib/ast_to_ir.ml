open Ast
open Ir

(* --- 1. 环境与辅助函数 --- *)

type env = {
  mutable temp_counter: int;  (* 用于生成新的临时变量 *)
  mutable label_counter: int; (* 用于生成新的标签 *)
  break_label: string option; (* 当前循环的结束标签 *)
  continue_label: string option; (* 当前循环的开始标签 *)
}


(* 创建初始环境 *)
let make_env () = {
  temp_counter = 0;
  label_counter = 0;
  break_label = None;
  continue_label = None;
}

(* 生成一个新的临时变量操作数 *)
let new_temp env =
  let i = env.temp_counter in
  env.temp_counter <- i + 1;
  Temp i

(* 生成一个新的标签字符串 *)
let new_label env =
  let i = env.label_counter in
  env.label_counter <- i + 1;
  "L" ^ string_of_int i

(* --- 2. 表达式生成 --- *)

(*
 * gen_expr: 将 AST 表达式转换为 IR 指令序列
 * @param env - 当前环境
 * @param expr - 要转换的 AST 表达式
 * @return (operand, instr list) - 一个元组，包含:
 *   - 存放表达式结果的操作数 (可能是 Const, Temp, 或 Name)
 *   - 为计算该结果而生成的指令列表
 *)
let rec gen_expr env expr : operand * instr list =
  match expr with
  | IntLit n ->
      (* 常量直接返回 Const 操作数，不产生任何指令 *)
      (Const n, [])

  | Var id ->
      (* 变量需要从内存加载到临时变量中 *)
      let dest_temp = new_temp env in
      let load_instr = Load { dest = dest_temp; src_addr = Name id } in
      (dest_temp, [load_instr])

  | UnOp (op, e) ->
      let (e_op, e_instrs) = gen_expr env e in
      let dest_temp = new_temp env in
      let unop_instr = UnOp { dest = dest_temp; op = op; src = e_op } in
      (dest_temp, e_instrs @ [unop_instr])

  | BinOp (op, e1, e2) ->
      let (e1_op, e1_instrs) = gen_expr env e1 in
      let (e2_op, e2_instrs) = gen_expr env e2 in
      let dest_temp = new_temp env in
      let binop_instr = BinOp { dest = dest_temp; op = op; src1 = e1_op; src2 = e2_op } in
      (dest_temp, e1_instrs @ e2_instrs @ [binop_instr])

  | Call (fname, args) ->
      (* 依次计算所有参数，并收集它们的指令和结果操作数 *)
      let (arg_ops, arg_instrs_list) =
        List.map (gen_expr env) args |> List.split
      in
      let all_arg_instrs = List.flatten arg_instrs_list in

      (* 返回值需要存放在一个新的临时变量中 *)
      let dest_temp = new_temp env in
      let call_instr = Call { dest = Some dest_temp; name = fname; args = arg_ops } in
      (dest_temp, all_arg_instrs @ [call_instr])


(* --- 3. 语句生成 --- *)

(*
 * gen_stmt: 将 AST 语句转换为 IR 指令序列
 * @param env - 当前环境
 * @param stmt - 要转换的 AST 语句
 * @return instr list - 生成的指令列表
 *)
let rec gen_stmt env stmt : instr list =
  match stmt with
  | Block stmts ->
      (* 将块内所有语句生成的指令列表连接起来 *)
      List.map (gen_stmt env) stmts |> List.flatten

  | Expr e ->
      (* 计算表达式，但忽略其结果。处理`func();` 这样的调用 *)
      let (_, instrs) = gen_expr env e in
      instrs

  | VarDecl (_, _, _) ->
      (* 变量声明暂时不生成 IR 指令。
         内存分配（如栈帧调整）在后端代码生成阶段处理。
         带初始化由 Assign 语句处理。
     *)
      []

  | Assign (id, e) ->
      (* 计算右侧表达式 *)
      let (e_op, e_instrs) = gen_expr env e in
      (* 生成 Store 指令将结果存回变量 *)
      let store_instr = Store { dest_addr = Name id; src = e_op } in
      e_instrs @ [store_instr]

  | If (cond, then_stmt, else_opt) ->
      let label_true = new_label env in
      let label_false = new_label env in
      let label_end = new_label env in

      let (cond_op, cond_instrs) = gen_expr env cond in
      let cjump_instr = CJump { cond = cond_op; label_true; label_false } in

      let then_instrs = gen_stmt env then_stmt in
      
      (match else_opt with
      | Some else_s ->
          let else_instrs = gen_stmt env else_s in
          cond_instrs
          @ [cjump_instr]
          @ [Label label_true] @ then_instrs @ [Jump label_end]
          @ [Label label_false] @ else_instrs
          @ [Label label_end]
      | None ->
          cond_instrs
          @ [cjump_instr]
          @ [Label label_true] @ then_instrs
          @ [Label label_false] (* 如果没有 else，false 分支就是 if 语句的结尾 *)
      )

  | While (cond, body) ->
      let label_start = new_label env in
      let label_body = new_label env in
      let label_end = new_label env in

      let (cond_op, cond_instrs) = gen_expr env cond in
      let cjump_instr = CJump { cond = cond_op; label_true = label_body; label_false = label_end } in
      
      let body_instrs = gen_stmt env body in

      [Label label_start]
      @ cond_instrs
      @ [cjump_instr]
      @ [Label label_body]
      @ body_instrs
      @ [Jump label_start] (* 循环回到开始处重新判断条件 *)
      @ [Label label_end]

  | Break ->
      (* 这是一个简化的实现。一个完整的实现需要环境知道当前循环的
         'end' 标签是什么。我们暂时假定它不生成指令，由优化阶段处理，
         或者需要更复杂的环境来传递标签。*)
      [] 

  | Continue ->
      (* 同 Break *)
      [] 

  | Return e_opt ->
      (match e_opt with
      | Some e ->
          let (e_op, e_instrs) = gen_expr env e in
          e_instrs @ [Return (Some e_op)]
      | None -> [Return None]
      )

  | EmptyStmt -> []


(* --- 4. 顶层转换函数 --- *)

(* 将单个 AST 函数定义转换为 IR 函数定义 *)
let gen_func_def (fdef: Ast.func_def) : ir_func =
  let env = make_env () in
  let body_instrs = gen_stmt env fdef.body in
  {
    name = fdef.fname;
    params = List.map (fun p -> p.pname) fdef.params;
    body = body_instrs;
  }

(* 程序的总入口：将整个 AST 编译单元转换为 IR 程序 *)
let generate (prog: Ast.comp_unit) : ir_program =
  List.map gen_func_def prog
