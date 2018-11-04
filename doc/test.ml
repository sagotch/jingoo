open Jingoo

module Json = struct

  open Jg_types

  let rec of_json = function
    | `Assoc o -> Tobj (List.map (fun (k, v) -> (k, of_json v)) o)
    | `Bool b -> Tbool b
    | `Float f -> Tfloat f
    | `Int i -> Tint i
    | `List l -> Tlist (List.map of_json l)
    | `String s -> Tstr s
    | `Null -> Tnull

end

let file_contents file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.unsafe_to_string s

let ls dir filter =
  List.filter filter @@
    let rec loop result = function
      | f :: fs when Sys.is_directory f ->
         Sys.readdir f
         |> Array.to_list
         |> List.rev_map (Filename.concat f)
         |> List.rev_append fs
         |> loop result
      | f::fs -> loop (f :: result) fs
      | []    -> result
    in
    loop [] [dir]

let test jingoo_f =
  let html_f = Filename.remove_extension jingoo_f ^ ".expected" in
  let json_f = Filename.remove_extension jingoo_f ^ ".json" in
  let models =
    if Sys.file_exists json_f then
      Yojson.Basic.from_string (file_contents json_f)
      |> Json.of_json
      |> Jg_types.unbox_obj
    else []
  in
  let expected = file_contents html_f in
  let res = Jg_template.from_file ~models jingoo_f in
  if res <> expected then begin
      prerr_endline @@ "--- Error: " ^ jingoo_f ;
      prerr_endline @@ "--- Expected: " ;
      prerr_endline @@ expected ;
      prerr_endline @@ "--- Got: " ;
      prerr_endline @@ res ;
      failwith jingoo_f ;
    end
  else prerr_endline @@ "--- OK: " ^ jingoo_f

let () =
  let jingoo = ls (Sys.getenv "DOC_SAMPLE_DIR") (fun f -> Filename.extension f = ".jingoo") in
  List.iter test (List.sort compare jingoo)
