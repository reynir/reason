open Migrate_parsetree
open Ast_404
type q = Parsetree.signature_item
type t = Parsetree.signature
let err = Printer_maker.err 
let defaultInterfaceParserFor use_stdin filename =
  let open Reason_toolchain in
    let (theParser,parsedAsML) =
      if Filename.check_suffix filename ".rei"
      then (RE.interface_with_comments, false)
      else
        if Filename.check_suffix filename ".mli"
        then (ML.interface_with_comments, true)
        else
          err
            ("Cannot determine default interface parser for filename '" ^
               (filename ^ "'."))
       in
    ((theParser (setup_lexbuf use_stdin filename)), parsedAsML, true)
  
let parse ~use_stdin  filetype filename =
  let ((ast,comments),parsedAsML,parsedAsInterface) =
    match filetype with
    | `Auto -> defaultInterfaceParserFor use_stdin filename
    | `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
    | `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename
    | `ML ->
        let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename  in
        let intf = Reason_toolchain.ML.interface_with_comments  in
        ((intf lexbuf), true, true)
    | `Reason ->
        let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename  in
        let intf = Reason_toolchain.RE.interface_with_comments  in
        ((intf lexbuf), false, true)
     in
  if not parsedAsInterface
  then err "The file parsed does not appear to be an interface file."
  else ((ast, comments), parsedAsML) 
let print printtype filename parsedAsML output_chan output_formatter =
  match printtype with
  | `BinaryReason ->
      (fun (ast,comments)  ->
         output_value output_chan
           (Config.ast_intf_magic_number, filename, ast, comments,
             parsedAsML, true))
  | `Binary ->
      (fun (ast,comments)  ->
         Ast_io.to_channel output_chan filename
           (Ast_io.Intf
              ((module OCaml_current),
                (Reason_toolchain.To_current.copy_signature ast))))
  | `AST ->
      (fun (ast,comments)  ->
         Printast.interface output_formatter
           (Reason_toolchain.To_current.copy_signature ast))
  | `None -> (fun (ast,comments)  -> ())
  | `ML -> Reason_toolchain.ML.print_interface_with_comments output_formatter
  | `Reason ->
      Reason_toolchain.RE.print_interface_with_comments output_formatter
  