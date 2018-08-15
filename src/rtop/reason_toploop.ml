let () =
  if
    (List.exists ((=) "camlp4o") (!Topfind.predicates)) ||
      (List.exists ((=) "camlp4r") (!Topfind.predicates))
  then print_endline "Reason is incompatible with camlp4!"
  else
    (Toploop.parse_toplevel_phrase :=
       (Reason_util.correctly_catch_parse_errors
          (fun x  ->
             Reason_toolchain.To_current.copy_toplevel_phrase
               (Reason_toolchain.RE.toplevel_phrase x)));
     Toploop.parse_use_file :=
       (Reason_util.correctly_catch_parse_errors
          (fun x  ->
             List.map Reason_toolchain.To_current.copy_toplevel_phrase
               (Reason_toolchain.RE.use_file x))))
  