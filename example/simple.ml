open Jingoo

let file = ref "cheatsheet.jingoo"

let () =
  Arg.parse [
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
  ] ignore "";

  let result_string =
    Jg_template.from_file !file ~models:Test_data.models in

  print_endline result_string
