open Odoc_info
module Naming = Odoc_html.Naming
open Odoc_info.Value
open Odoc_info.Module
open Odoc_info.Extension
open Odoc_info.Exception
open Odoc_info.Type
open Odoc_info.Class
let p = Printf.bprintf 
let bp = Printf.bprintf 
let bs = Buffer.add_string 
let wrap f g fmt x = g fmt (f x) 
let () =
  let open Reason_toolchain.From_current in
    Oprint.out_value := (wrap copy_out_value Reason_oprint.print_out_value);
    Oprint.out_type := (wrap copy_out_type Reason_oprint.print_out_type);
    Oprint.out_class_type :=
      (wrap copy_out_class_type Reason_oprint.print_out_class_type);
    Oprint.out_module_type :=
      (wrap copy_out_module_type Reason_oprint.print_out_module_type);
    Oprint.out_sig_item :=
      (wrap copy_out_sig_item Reason_oprint.print_out_sig_item);
    Oprint.out_signature :=
      (wrap (List.map copy_out_sig_item) Reason_oprint.print_out_signature);
    Oprint.out_type_extension :=
      (wrap copy_out_type_extension Reason_oprint.print_out_type_extension);
    Oprint.out_phrase :=
      (wrap copy_out_phrase Reason_oprint.print_out_phrase)
  
module Html = (val
  (match !Odoc_args.current_generator with
   | None  -> ((module
       Odoc_html.Generator) : (module Odoc_html.Html_generator))
   | Some (Odoc_gen.Html m) -> m
   | _ ->
       failwith
         "A non-html generator is already set. Cannot install the Todo-list html generator" : 
  (module Odoc_html.Html_generator)))
let raw_string_of_type_list sep type_list =
  let buf = Buffer.create 256  in
  let fmt = Format.formatter_of_buffer buf  in
  let rec need_parent t =
    match t.Types.desc with
    | Types.Tarrow _|Types.Ttuple _ -> true
    | Types.Tlink t2|Types.Tsubst t2 -> need_parent t2
    | Types.Tconstr _ -> false
    | Types.Tvar _|Types.Tunivar _|Types.Tobject _|Types.Tpoly _|Types.Tfield
      _|Types.Tnil |Types.Tvariant _|Types.Tpackage _ -> false
     in
  let print_one_type variance t =
    Printtyp.mark_loops t;
    if need_parent t
    then
      (Format.fprintf fmt "(%s" variance;
       Printtyp.type_scheme_max ~b_reset_names:false fmt t;
       Format.fprintf fmt ")")
    else
      (Format.fprintf fmt "%s" variance;
       Printtyp.type_scheme_max ~b_reset_names:false fmt t)
     in
  (match type_list with
   | [] -> ()
   | (variance,ty)::[] -> print_one_type variance ty
   | (variance,ty)::tyl ->
       (Format.fprintf fmt "@[<hov 2>";
        print_one_type variance ty;
        List.iter
          (fun (variance,t)  ->
             Format.fprintf fmt "@,%s" sep; print_one_type variance t) tyl;
        Format.fprintf fmt "@]"));
  Format.pp_print_flush fmt ();
  Buffer.contents buf 
let string_of_type_param_list t =
  Printf.sprintf "%s"
    (raw_string_of_type_list " "
       (List.map
          (fun (typ,co,cn)  ->
             ((Odoc_str.string_of_variance t (co, cn)), typ))
          t.Odoc_type.ty_parameters))
  
let string_of_type_extension_param_list te =
  Printf.sprintf "%s"
    (raw_string_of_type_list " "
       (List.map (fun typ  -> ("", typ)) te.Odoc_extension.te_type_parameters))
  
let string_of_value v =
  let module M = Odoc_value in
    "let " ^
      ((Name.simple v.M.val_name) ^
         (" : " ^
            ((Odoc_print.string_of_type_expr v.M.val_type) ^
               ("\n" ^
                  (match v.M.val_info with
                   | None  -> ""
                   | Some i -> Odoc_misc.string_of_info i)))))
  