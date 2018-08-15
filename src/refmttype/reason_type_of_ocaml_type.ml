let () =
  Reason_pprint_ast.configure ~width:80 ~assumeExplicitArity:false
    ~constructorLists:[]
  
let reasonFormatter = Reason_pprint_ast.createFormatter () 
let parseAsCoreType str formatter =
  ((Lexing.from_string str) |> Reason_toolchain.ML.core_type) |>
    (reasonFormatter#core_type formatter)
  
let parseAsImplementation str formatter =
  ((Lexing.from_string str) |> Reason_toolchain.ML.implementation) |>
    (reasonFormatter#structure [] formatter)
  
let parseAsInterface str formatter =
  ((Lexing.from_string str) |> Reason_toolchain.ML.interface) |>
    (reasonFormatter#signature [] formatter)
  
let parseAsCoreModuleType str formatter =
  ((Lexing.from_string ("module X: " ^ str)) |> Reason_toolchain.ML.interface)
    |> (reasonFormatter#signature [] formatter)
  
let parseAsWeirdListSyntax str a =
  if str = "type 'a list = [] | :: of 'a * 'a list"
  then "type list 'a = [] | :: of list 'a 'a"
  else raise (Syntaxerr.Error a) 
let convert str =
  let formatter = Format.str_formatter  in
  try parseAsCoreType str formatter; Format.flush_str_formatter ()
  with
  | Syntaxerr.Error _ ->
      (try parseAsImplementation str formatter; Format.flush_str_formatter ()
       with
       | Syntaxerr.Error _ ->
           (try parseAsInterface str formatter; Format.flush_str_formatter ()
            with
            | Syntaxerr.Error _ ->
                (try
                   parseAsCoreModuleType str formatter;
                   Format.flush_str_formatter ()
                 with | Syntaxerr.Error a -> parseAsWeirdListSyntax str a)))
  