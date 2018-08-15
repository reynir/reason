open Lexing
module Cmdliner = Vendored_cmdliner
open Cmdliner
let read_lines file =
  let list = ref []  in
  let chan = open_in file  in
  try while true do list := ((input_line chan) :: (!list)) done; []
  with | End_of_file  -> (close_in chan; List.rev (!list)) 
let refmt interface is_recoverable explicit_arity parse_ast print print_width
  heuristics_file in_place input_files =
  let refmt_single input_file =
    let (use_stdin,input_file) =
      match input_file with
      | Some name -> (false, name)
      | None  -> (true, "")  in
    let parse_ast =
      match (parse_ast, use_stdin) with
      | (Some x,_) -> x
      | (None ,false ) -> `Auto
      | (None ,true ) -> `Reason  in
    let constructorLists =
      match heuristics_file with
      | Some f_name -> read_lines f_name
      | None  -> []  in
    let interface =
      match interface with
      | true  -> true
      | false  ->
          (Filename.check_suffix input_file ".rei") ||
            (Filename.check_suffix input_file ".mli")
       in
    let output_file =
      match (in_place, use_stdin) with
      | (true ,true ) -> Printer_maker.err "Cannot write in place to stdin."
      | (true ,_) -> Some input_file
      | (false ,_) -> None  in
    let ((module Printer)  : (module Printer_maker.PRINTER)) =
      if interface
      then (module Reason_interface_printer)
      else (module Reason_implementation_printer)  in
    Reason_config.configure ~r:is_recoverable;
    Location.input_name := input_file;
    (let _ =
       Reason_pprint_ast.configure ~width:print_width
         ~assumeExplicitArity:explicit_arity ~constructorLists
        in
     let (ast,parsedAsML) = Printer.parse ~use_stdin parse_ast input_file  in
     let output_chan = Printer_maker.prepare_output_file output_file  in
     let output_formatter = Format.formatter_of_out_channel output_chan  in
     Printer.print print input_file parsedAsML output_chan output_formatter
       ast;
     Format.pp_print_flush output_formatter ();
     flush output_chan;
     Printer_maker.close_output_file output_file output_chan)
     in
  try
    match input_files with
    | [] -> `Ok (refmt_single None)
    | ls ->
        `Ok (List.iter (fun file  -> refmt_single (Some file)) input_files)
  with | Printer_maker.Invalid_config msg -> `Error (true, msg)
  | exn -> (Location.report_exception Format.err_formatter exn; exit 1) 
let top_level_info =
  let doc = "Reason's Parser & Pretty-printer"  in
  let man =
    [`S "DESCRIPTION";
    `P
      "refmt lets you format Reason files, parse them, and convert them between OCaml syntax and Reason syntax."]
     in
  let version =
    "Reason " ^ (Package.version ^ (" @ " ^ Package.git_short_version))  in
  Term.info "refmt" ~version ~doc ~man 
let refmt_t =
  let open Term in
    let open Refmt_args in
      (((((((((const refmt) $ interface) $ recoverable) $ explicit_arity) $
             parse_ast)
            $ print)
           $ print_width)
          $ heuristics_file)
         $ in_place)
        $ input
  
let () =
  match Term.eval ((Term.ret refmt_t), top_level_info) with
  | `Error s -> exit 1
  | _ -> exit 0 