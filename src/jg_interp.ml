(*
  jg_interp.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types
open Jg_utils
open Jg_runtime

let rec filter_map fn = function
  | [] -> []
  | hd :: tl ->
    match fn hd with
    | Some x -> x :: filter_map fn tl
    | None -> filter_map fn tl

let rec value_of_expr env ctx = function
  | LiteralExpr(x) -> x
  | IdentExpr(name) -> jg_get_value ctx name
  | NotOpExpr(expr) -> jg_not @@ value_of_expr env ctx expr
  | NegativeOpExpr(expr) -> jg_negative @@ value_of_expr env ctx expr
  | PlusOpExpr(left, right) -> jg_plus (value_of_expr env ctx left) (value_of_expr env ctx right)
  | MinusOpExpr(left, right) -> jg_minus (value_of_expr env ctx left) (value_of_expr env ctx right)
  | TimesOpExpr(left, right) -> jg_times (value_of_expr env ctx left) (value_of_expr env ctx right)
  | PowerOpExpr(left, right) -> jg_power (value_of_expr env ctx left) (value_of_expr env ctx right)
  | DivOpExpr(left, right) -> jg_div (value_of_expr env ctx left) (value_of_expr env ctx right)
  | ModOpExpr(left, right) -> jg_mod (value_of_expr env ctx left) (value_of_expr env ctx right)
  | AndOpExpr(left, right) ->
    Tbool (jg_is_true (value_of_expr env ctx left) && jg_is_true (value_of_expr env ctx right))
  | OrOpExpr(left, right) ->
    Tbool (jg_is_true (value_of_expr env ctx left) || jg_is_true (value_of_expr env ctx right))
  | EqEqOpExpr(left, right) -> jg_eq_eq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | NotEqOpExpr(left, right) -> jg_not_eq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | LtOpExpr(left, right) -> jg_lt (value_of_expr env ctx left) (value_of_expr env ctx right)
  | GtOpExpr(left ,right) -> jg_gt (value_of_expr env ctx left) (value_of_expr env ctx right)
  | LtEqOpExpr(left, right) -> jg_lteq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | GtEqOpExpr(left, right) -> jg_gteq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | InOpExpr(left, right) -> jg_inop (value_of_expr env ctx left) (value_of_expr env ctx right)
  | ListExpr(expr_list) -> Tlist (List.map (value_of_expr env ctx) expr_list)
  | SetExpr(expr_list) -> Tset (List.map (value_of_expr env ctx) expr_list)
  | DotExpr(IdentExpr(name), prop) -> jg_obj_lookup_by_name ctx name prop
  | DotExpr(left, prop) -> jg_obj_lookup (value_of_expr env ctx left) prop
  | BracketExpr(left, expr) ->
    (match value_of_expr env ctx expr with
     | Tstr prop -> jg_obj_lookup (value_of_expr env ctx left) prop
     | _ -> Tnull)
  | TestOpExpr(IdentExpr(name), IdentExpr("defined")) -> jg_test_defined ctx name
  | TestOpExpr(IdentExpr(name), IdentExpr("undefined")) -> jg_test_undefined ctx name
  | TestOpExpr(DotExpr(IdentExpr(name), prop), IdentExpr("defined")) -> jg_test_obj_defined ctx name prop
  | TestOpExpr(DotExpr(IdentExpr(name), prop), IdentExpr("undefined")) -> jg_test_obj_undefined ctx name prop
  | TestOpExpr(BracketExpr(IdentExpr(name), expr), IdentExpr("defined")) ->
    (match value_of_expr env ctx expr with
     | Tstr prop -> jg_test_obj_defined ctx name prop
     | _ -> Tbool false)
  | TestOpExpr(BracketExpr(IdentExpr(name), expr), IdentExpr("undefined")) ->
    (match value_of_expr env ctx expr with
     | Tstr prop -> jg_test_obj_undefined ctx name prop
     | _ -> Tbool true)
  | TestOpExpr(IdentExpr(name), IdentExpr("none")) -> jg_test_none ctx name
  | TestOpExpr(IdentExpr(name), IdentExpr("escaped")) -> jg_test_escaped ctx
  | TestOpExpr(IdentExpr(name), IdentExpr("upper")) -> jg_test_upper (jg_get_value ctx name) []
  | TestOpExpr(IdentExpr(name), IdentExpr("lower")) -> jg_test_lower (jg_get_value ctx name) []
  | TestOpExpr(target, test) -> jg_apply (value_of_expr env ctx test) [value_of_expr env ctx target]

  | ObjExpr(expr_list) ->
    Tobj (List.map (function
      | IdentExpr(name), expr -> (name, value_of_expr env ctx expr)
      | LiteralExpr(Tstr(name)), expr -> (name, value_of_expr env ctx expr)
      | other, expr -> failwith "invalid object syntax"
    ) expr_list)

  | ApplyExpr(IdentExpr("eval"), [expr]) ->
    let buffer = Buffer.create 256 in
    let ctx = {ctx with output = Buffer.add_string buffer } in
    let str = string_of_tvalue @@ value_of_expr env ctx expr in
    let statements = statements_from_string ctx str in
    let _ = List.fold_left (eval_statement env) ctx statements in
    Tstr (Buffer.contents buffer)

  | ApplyExpr(expr, args) ->
    let name = apply_name_of expr in
    let nargs = nargs_of env ctx args in
    let kwargs = kwargs_of env ctx args in
    let callable = value_of_expr env ctx expr in
    (match callable with
      | Tfun fn ->
	(match nargs with
	  | [] -> Tfun (fun args _ -> fn args kwargs)
	  | _ -> jg_apply callable nargs ~kwargs ~name)
      | _ ->
	(match jg_get_macro ctx name with
	  | Some macro -> ignore @@ eval_macro env ctx name nargs kwargs macro; Tnull
	  | None -> Tnull))

  | expr -> failwith @@ spf "syntax error: value_of_expr:%s" (dump_expr expr)

and apply_name_of = function
  | IdentExpr(name) -> name
  | DotExpr(IdentExpr(name), prop) -> spf "%s.%s" name prop
  | ApplyExpr(expr, args) -> apply_name_of expr
  | _ -> "<lambda>"

and ident_names_of lst =
  filter_map
    (function IdentExpr name -> Some name | _ -> None)
    lst

and alias_names_of lst =
  List.map (function
    | IdentExpr(name) -> (name, name)
    | AliasExpr(IdentExpr(name), IdentExpr(name')) -> (name, name')
    | _ -> failwith "invalid argument:alias_names_of") lst

and nargs_of env ctx args =
  filter_map
    (function KeywordExpr _ -> None | x -> Some (value_of_expr env ctx x))
    args

and kwargs_of env ctx args =
  filter_map
    (function KeywordExpr(IdentExpr(name), expr) -> Some (name, value_of_expr env ctx expr)
            | _ -> None)
    args

and eval_macro env ctx name args kwargs macro =
  let caller = match jg_get_macro ctx "caller" with None -> false | _ -> true in
  jg_eval_macro env ctx name args kwargs macro ~caller:caller (fun ctx statements ->
    List.fold_left (eval_statement env) ctx statements
  )

and is_safe_expr = function
  | ApplyExpr(IdentExpr("safe"), _) -> true
  | ApplyExpr(expr, _) -> is_safe_expr expr
  | _ -> false

and eval_statement env ctx = function
  | TextStatement(text) ->
    jg_output ctx (Tstr text) ~safe:true

  | ExpandStatement(expr) ->
    jg_output ctx (value_of_expr env ctx expr) ~autoescape:env.autoescape ~safe:(is_safe_expr expr)

  | SetStatement(SetExpr(ident_list), expr) ->
    jg_bind_names ctx (ident_names_of ident_list) (value_of_expr env ctx expr)

  | SetStatement(DotExpr(IdentExpr ns, v), expr) ->
    Hashtbl.add
      (Hashtbl.find ctx.namespace_table ns) v (value_of_expr env ctx expr) ;
    ctx

  | FilterStatement(IdentExpr(name), statements) ->
    let ctx = jg_set_filter ctx name in
    let ctx = List.fold_left (eval_statement env) ctx statements in
    jg_pop_filter ctx

  | IfStatement(if_elseif_conds, false_statements) ->
    let rec select_case = function
      | (expr, statements) :: rest when jg_is_true (value_of_expr env ctx expr) -> statements
      | h :: rest -> select_case rest
      | [] -> false_statements in
    List.fold_left (eval_statement env) ctx @@ select_case if_elseif_conds

  | ForStatement(iterator, iterable_expr, statements) ->
    let iterator =
      match iterator with
	| IdentExpr(name) -> [name]
	| SetExpr(lst) -> ident_names_of lst
	| _ -> failwith "invalid iterator" in
    let iterable = value_of_expr env ctx iterable_expr in
    let is_iterable = Jg_runtime.is_iterable iterable in
    (* [ISSUE#23] when strict_mode is enabled, raises error if loop target is not iterable. *)
    if env.strict_mode = true && is_iterable = false then
      failwith @@ spf "%s is not iterable" (string_of_tvalue iterable)
    ;
    jg_iter ctx iterator (fun ctx ->
      ignore @@ List.fold_left (eval_statement env) ctx statements
    ) iterable;
    ctx

  | BlockStatement(IdentExpr(name), statements) ->
    List.fold_left (eval_statement env) ctx statements

  | CallStatement(IdentExpr(name), call_args_def, macro_args, call_statements) ->
    (match jg_get_macro ctx name with
      | Some(Macro(arg_names, def_kwargs, macro_statements)) ->
	let call_arg_names = ident_names_of call_args_def in
	let call_defaults = kwargs_of env ctx call_args_def in
	let ctx = jg_set_macro ctx "caller" @@ Macro(call_arg_names, call_defaults, call_statements) in
	let text = string_of_tvalue @@ value_of_expr env ctx @@ ApplyExpr(IdentExpr(name), macro_args) in
	let ctx = jg_remove_macro ctx "caller" in
	jg_output ctx (Tstr text) ~safe:true

      | None -> ctx (* do nothing *)
    )

  | IncludeStatement(IdentExpr(name), with_context) ->
    eval_statement env ctx @@ IncludeStatement(LiteralExpr(jg_get_value ctx name), with_context)

  | IncludeStatement(LiteralExpr(Tstr path), true) ->
    let statements = statements_from_file env ctx path in
    List.fold_left (eval_statement env) ctx statements

  | IncludeStatement(LiteralExpr(Tstr path), false) ->
    let ctx' = jg_init_context ctx.output env in
    let statements = statements_from_file env ctx' path in
    let _ = List.fold_left (eval_statement env) ctx' statements in
    ctx

  | RawIncludeStatement(IdentExpr(name)) ->
    eval_statement env ctx @@ RawIncludeStatement(LiteralExpr(jg_get_value ctx name))

  | RawIncludeStatement(LiteralExpr(Tstr path)) ->
    let file_path = get_file_path env ctx path in
    let source = Jg_utils.read_file_as_string file_path in
    jg_output ctx (Tstr source) ~safe:true

  | WithStatement(binds, statements) ->
    let kwargs = kwargs_of env ctx binds in
    let names, values = List.split kwargs in
    let ctx = jg_push_frame ctx in
    let ctx = jg_set_values ctx names values in
    let ctx = List.fold_left (eval_statement env) ctx statements in
    jg_pop_frame ctx

  | AutoEscapeStatement(expr, statements) ->
    let ctx =
      match value_of_expr env ctx expr with
	| Tbool true ->
	  jg_set_filter ctx "escape"
	| Tbool false ->
	  jg_set_filter ctx "safe"
	| _ ->
	  failwith "invalid syntax:autoescape(bool value required)" in
    let ctx = List.fold_left (eval_statement env) ctx statements in
    jg_pop_filter ctx

  | NamespaceStatement (ns, assign) ->
    let size = match List.length assign with 0 -> 10 | x -> x in
    let h = Hashtbl.create size in
    List.iter (fun (k, v) -> Hashtbl.add h k (value_of_expr env ctx v)) assign ;
    Hashtbl.add ctx.namespace_table ns h ;
    ctx

  | _ -> ctx

and unfold_extends env ctx stmts =
  let rec iter ret = function
    | ExtendsStatement(path) :: rest ->
      let statements = unfold_extends env ctx @@ statements_from_file env ctx path in
      iter (ret @ statements) rest
    | other :: rest -> iter (ret @ [other]) rest
    | [] -> ret in
  iter [] stmts

and align_block stmts =
  let is_same_block name = function
    | BlockStatement(IdentExpr(name'),_) -> name' = name
    | _ -> false in
  let erase_block name lst =
    List.filter (fun x -> not (is_same_block name x)) lst in
  let rec iter ret = function
    | (BlockStatement(IdentExpr(name), _) as block) :: rest ->
      (try
	 let block' = List.find (is_same_block name) rest in
	 iter (block' :: ret) (erase_block name rest)
       with
	   Not_found -> iter (block :: ret) rest)
    | other :: rest -> iter (other :: ret) rest
    | [] -> List.rev ret in

  iter [] stmts

and reset_interp () =
  Parsing.clear_parser ()

and import_macro ?namespace ?select env ctx codes =
  let macro_name name = match namespace with Some namespace -> spf "%s.%s" namespace name | _ -> name in
  let alias_name name = match select with None -> name | Some alist -> List.assoc name alist in
  let can_import name = match select with None -> true | Some alist -> List.mem_assoc name alist in
  List.fold_left (fun ctx code ->
    match code with
      | MacroStatement(IdentExpr(name), def_args, statements) when can_import name ->
	let arg_names = ident_names_of def_args in
	let kwargs = kwargs_of env ctx def_args in
	jg_set_macro ctx (macro_name @@ alias_name name) @@ Macro(arg_names, kwargs, statements)

      | BlockStatement(_, stmts) ->
	import_macro env ctx ?namespace ?select stmts

      | IncludeStatement(LiteralExpr(Tstr path), _) ->
	import_macro env ctx ?namespace ?select @@ statements_from_file env ctx path

      | ImportStatement(path, namespace) ->
	import_macro env ctx ?namespace ?select @@ statements_from_file env ctx path

      | FromImportStatement(path, select_macros) ->
	let alias_names = alias_names_of select_macros in
	import_macro env ctx ?namespace ?select:(Some alias_names) @@ statements_from_file env ctx path

      | _ -> ctx
  ) ctx codes

and get_file_path env ctx file_name =
  Jg_utils.get_file_path file_name ~template_dirs:env.template_dirs

and statements_from_file env ctx file_name =
  let file_path = get_file_path env ctx file_name in
  let source = Jg_utils.read_file_as_string file_path in
  statements_from_string ctx source ~file_path

and statements_from_string ?file_path ctx source =
  let lexbuf = Lexing.from_string source in
  Jg_lexer.reset_context ();
  Jg_lexer.init_lexer_pos file_path lexbuf;
  try
    Jg_parser.input Jg_lexer.main lexbuf
  with
      exn ->
	raise @@ SyntaxError(Jg_utils.get_parser_error exn lexbuf)

and init_context ?(env=std_env) ?(models=[]) ~output () =
  let extensions = env.extensions in
  jg_load_extensions extensions;
  jg_init_context ~models output env

and from_file
    ?(env=std_env) ?(models=[]) ~output
    ?(ctx = init_context ~env ~models ~output ())
    file_name =
  let file_path = get_file_path env ctx file_name in
  let source = Jg_utils.read_file_as_string file_path in
  from_string source ~file_path ~env ~ctx ~models ~output

and from_string ?(env=std_env) ?(models=[]) ?file_path ~output
    ?(ctx = init_context ~env ~models ~output ())
    source =
  try
    let () = lock_unlock.lock () in
    let () = reset_interp () in
    let codes =
      statements_from_string ?file_path ctx source
      |> unfold_extends env ctx
      |> align_block
    in
    let ctx = import_macro env ctx codes in
    let _ = List.fold_left (eval_statement env) ctx codes in
    reset_interp ();
    lock_unlock.unlock ();
  with
      exn ->
	reset_interp ();
	lock_unlock.unlock ();
	raise exn
