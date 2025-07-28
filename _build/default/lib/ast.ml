(* ast.ml *)

(* 类型定义 *)
type typ =
  | IntType
  | VoidType

(* 二元运算符 *)
type binop =
  (* 算术运算符 *)
  | OpAdd | OpSub | OpMul | OpDiv | OpMod
  (* 关系运算符 *)
  | OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe
  (* 逻辑运算符 *)
  | OpAnd | OpOr

(* 一元运算符 *)
type unop =
  | OpNeg   (* 算术负号, -x *)
  | OpNot   (* 逻辑非, !x *)

(* 表达式 *)
type expr =
  | IntLit of int                 (* 整数常量 *)
  | Var of string                 (* 变量/标识符 *)
  | BinOp of binop * expr * expr  (* 二元运算 *)
  | UnOp of unop * expr           (* 一元运算 *)
  | Call of string * expr list    (* 函数调用 *)

(* 语句 *)
type stmt =
  | Block of stmt list            (* 语句块 {...} *)
  | EmptyStmt                     (* 空语句 ; *)
  | Expr of expr                  (* 表达式语句 *)
  | VarDecl of typ * string * expr option     (*变量声明，可以同时表示 'int a;' (None) 和 'int a = 5;' (Some (IntLit 5)*)
  | Assign of string * expr       (* 赋值语句 *)
  | If of expr * stmt * stmt option (* if-else, else 分支可选 *)
  | While of expr * stmt          (* while 循环 *)
  | Break   (*break*)
  | Continue    (*continue*)
  | Return of expr option         (* return, 返回值可选，以支持 'return;' 和 'return 5;' *)


(* 函数参数 *)
type param = {
  ptyp : typ;
  pname : string;
}

(* 函数定义 *)
type func_def = {
  ftyp : typ;                    (* 返回类型 *)
  fname : string;                  (* 函数名 *)
  params : param list;             (* 参数列表 *)
  body : stmt;                     (* 函数体 *)
}

(* 编译单元 *)
(* 整个程序的顶层结构，即一个函数定义的列表。 *)
type comp_unit = func_def list


(* 输出 *)
(* let ast_of_typ = function
  | IntType -> "IntType"
  | VoidType -> "VoidType"

let ast_of_binop = function
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod"
  | Eq -> "Eq" | Neq -> "Neq" | Lt -> "Lt" | Le -> "Le" | Gt -> "Gt" | Ge -> "Ge"
  | And -> "And" | Or -> "Or"

let ast_of_unop = function
  | Neg -> "Neg"
  | Not -> "Not"

let rec ast_of_expr = function
  | IntLit n -> Printf.sprintf "IntLit %d" n
  | Var s -> Printf.sprintf "Var \"%s\"" s
  | BinOp (op, e1, e2) ->
      Printf.sprintf "BinOp (%s, %s, %s)"
        (ast_of_binop op) (ast_of_expr e1) (ast_of_expr e2)
  | UnOp (op, e) ->
      Printf.sprintf "UnOp (%s, %s)" (ast_of_unop op) (ast_of_expr e)
  | Call (fname, args) ->
      let args_str = String.concat "; " (List.map ast_of_expr args) in
      Printf.sprintf "Call (\"%s\", [%s])" fname args_str
  | Noexpr -> "Noexpr"

let rec ast_of_stmt = function
  | Block stmts ->
      let stmts_str = String.concat "; " (List.map ast_of_stmt stmts) in
      Printf.sprintf "Block [%s]" stmts_str
  | Expr e -> Printf.sprintf "Expr (%s)" (ast_of_expr e)
  | VarDecl (t, id, e) ->
      Printf.sprintf "VarDecl (%s, \"%s\", %s)" (ast_of_typ t) id (ast_of_expr e)
  | Assign (id, e) ->
      Printf.sprintf "Assign (\"%s\", %s)" id (ast_of_expr e)
  | If (cond, then_stmt, else_stmt) ->
      let else_str = match else_stmt with
        | Some s -> Printf.sprintf "Some (%s)" (ast_of_stmt s)
        | None -> "None"
      in
      Printf.sprintf "If (%s, %s, %s)"
        (ast_of_expr cond) (ast_of_stmt then_stmt) else_str
  | While (cond, body) ->
      Printf.sprintf "While (%s, %s)" (ast_of_expr cond) (ast_of_stmt body)
  | Break -> "Break"
  | Continue -> "Continue"
  | Return e_opt ->
      let e_str = match e_opt with
        | Some e -> Printf.sprintf "Some (%s)" (ast_of_expr e)
        | None -> "None"
      in
      Printf.sprintf "Return (%s)" e_str
  | EmptyStmt -> "EmptyStmt"

let ast_of_param (id, t) =
  Printf.sprintf "(\"%s\", %s)" id (ast_of_typ t)

let ast_of_func f =
  let params_str = String.concat "; " (List.map ast_of_param f.params) in
  Printf.sprintf "Func {\n  ftyp = %s;\n  fname = \"%s\";\n  params = [%s];\n  body = %s\n}"
    (ast_of_typ f.ftyp) f.fname params_str (ast_of_stmt f.body)

let ast_of_program (p : comp_unit) =
  let funcs_str = String.concat ";\n\n" (List.map ast_of_func p) in
  Printf.sprintf "Program [\n%s\n]" funcs_str *)
