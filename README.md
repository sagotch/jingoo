# jingoo

## About jingoo

Jingoo is OCaml template engine almost compatible with Jinja2(python template engine).

## Install

### manual

```bash
make
sudo make install
```
### opam

```bash
opam install jingoo
```

## Difference between Jinja2 and Jingoo

1. i18n features are not supported yet.
2. Cause of language difference between ocaml and python,
   some of built-in filters are different from original one,
   especially orders of arguments and supported optional arguments etc.
3. Single line comment is not supported. Because single '#' is used very much especially in html.

## Usage

### Simple usage

```ocaml
open Jingoo

(* output from direct string template *)
let result = Jg_template.from_string "{{ msg }}" ~models:[("msg", Jg_types.Tstr "hello, world!")]

(* or output from file template *)
let result2 = Jg_template.from_file "hello.jingoo" ~models:[("msg", Jg_types.Tstr "hello, world!")]
```

### Custom filter example

Set your custom filter to `filters` field of environment.

```ocaml
open Jingoo

(* define custom filter 'to_mail' *)
let to_mail ?(kwargs=[]) ?(defaults=[]) value =
  let id = Jg_runtime.string_of_tvalue value in
  let domain = Jg_runtime.string_of_tvalue (Jg_runtime.jg_get_kvalue "domain" kwargs ~defaults) in
  Jg_types.Tstr (spf "%s@%s" id domain)

(* set your extension to 'filters' field *)
let env = {Jg_types.std_env with
  filters = [
    ("to_mail", Jg_runtime.func_arg1 (to_mail ~defaults:[
      ("domain", Jg_types.Tstr "gmail.com");
    ]));
  ]
} in

(* output 'foo@gmail.com' *)
Jg_template.from_string "{{id | to_mail(domain='gmail.com')}}" ~env ~models:[
  ("id", Jg_types.Tstr "foo")
]
```

### Dynlink filter example

1. Write your own filter(`my_ext.ml` for example) and add it by `Jg_stub.add`(namespace as `my_ext` and func_name as `to_md5` for example).

```ocaml
open Jingoo

let to_md5 ?(kwargs=[]) ?(defaults=[]) value =
  let seed = Jg_runtime.jg_get_kvalue "seed" kwargs ~defaults in
  match value, seed with
  | Jg_types.Tstr str, Jg_types.Tstr seed ->
     Jg_types.Tstr (Digest.to_hex (Digest.string (str ^ seed)))
  | _ -> Jg_types.Tnull

let () =
  Jg_stub.add_func ~namespace:"my_ext" ~func_name:"to_md5" (Jg_runtime.func_arg1 (to_md5 ~defaults:[
    ("seed", Jg_types.Tstr "");
  ]))
```

2. Compile it to `my_ext.cmxs` by `-shared` option.

```bash
ocamlfind ocamlopt -shared -o my_ext.cmxs my_ext.ml
```

3. Set `my_ext.cmxs` to `extensions` field of environment, and you can use your custom filter `my_ext.to_md5`.

```ocaml
open Jingoo

(* set your extension to 'extensions' field *)
let env = {Jg_types.std_env with
  extensions = [
    "my_ext.cmxs";
  ]
} in

(* output '3cb988a734183289506ab7738261c827' *)
Jg_template.from_string "{{msg | my_ext.to_md5(seed='aaa')}}" ~env ~models:[
  ("msg", Jg_types.Tstr "foo");
]
```

## Cheatsheet

See [samples](https://github.com/tategakibunko/jingoo/tree/master/example/samples) directory.

`*.jingoo` is template example and `*.expected` is expected string.

## Documentation

[http://tategakibunko.github.io/jingoo/](http://tategakibunko.github.io/jingoo/)

## Playground

[https://sagotch.github.io/try-jingoo/](https://sagotch.github.io/try-jingoo/)

## License

MIT License
