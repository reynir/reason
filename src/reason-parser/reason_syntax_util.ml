open Ast_404
open Asttypes
open Ast_mapper
open Parsetree
open Longident
let potentially_conflicts_with ~keyword  s =
  let s_length = String.length s  in
  let keyword_length = String.length keyword  in
  (s_length >= keyword_length) &&
    (try
       for i = 0 to keyword_length - 1 do
         if (keyword.[i]) <> (s.[i]) then raise Exit
       done;
       for i = keyword_length to s_length - 1 do
         if (s.[i]) <> '_' then raise Exit
       done;
       true
     with | Exit  -> false)
  [@@ocaml.doc
    " Check to see if the string `s` is made up of `keyword` and zero or more\n    trailing `_` characters. "]
let string_add_suffix x = x ^ "_"
  [@@ocaml.doc
    " Add/remove an appropriate suffix when mangling potential keywords "]
let string_drop_suffix x = String.sub x 0 ((String.length x) - 1) 
[@@@ocaml.text
  " What do these *_swap functions do? Here's an example: Reason code uses `!`\n    for logical not, while ocaml uses `not`. So, for converting between reason\n    and ocaml syntax, ocaml `not` converts to `!`, reason `!` converts to\n    `not`.\n\n    In more complicated cases where a reserved keyword exists in one syntax but\n    not the other, these functions translate any potentially conflicting\n    identifier into the same identifier with a suffix attached, or remove the\n    suffix when converting back. Two examples:\n\n    reason to ocaml:\n\n    pub: invalid in reason to begin with\n    pub_: pub\n    pub__: pub_\n\n    ocaml to reason:\n\n    pub: pub_\n    pub_: pub__\n    pub__: pub___\n\n    =====\n\n    reason to ocaml:\n\n    match: match_\n    match_: match__\n    match__: match___\n\n    ocaml to reason:\n\n    match: invalid in ocaml to begin with\n    match_: match\n    match__: match_\n"]
let reason_to_ml_swap =
  function
  | "!" -> "not"
  | "^" -> "!"
  | "++" -> "^"
  | "===" -> "=="
  | "==" -> "="
  | "\\!==" -> "!=="
  | "\\===" -> "==="
  | "!=" -> "<>"
  | "!==" -> "!="
  | x when
      (potentially_conflicts_with ~keyword:"match" x) ||
        ((potentially_conflicts_with ~keyword:"method" x) ||
           (potentially_conflicts_with ~keyword:"private" x))
      -> string_add_suffix x
  | x when
      (potentially_conflicts_with ~keyword:"switch_" x) ||
        ((potentially_conflicts_with ~keyword:"pub_" x) ||
           (potentially_conflicts_with ~keyword:"pri_" x))
      -> string_drop_suffix x
  | everything_else -> everything_else 
let ml_to_reason_swap =
  function
  | "not" -> "!"
  | "!" -> "^"
  | "^" -> "++"
  | "==" -> "==="
  | "=" -> "=="
  | "!==" -> "\\!=="
  | "===" -> "\\==="
  | "<>" -> "!="
  | "!=" -> "!=="
  | x when
      (potentially_conflicts_with ~keyword:"match_" x) ||
        ((potentially_conflicts_with ~keyword:"method_" x) ||
           (potentially_conflicts_with ~keyword:"private_" x))
      -> string_drop_suffix x
  | x when
      (potentially_conflicts_with ~keyword:"switch" x) ||
        ((potentially_conflicts_with ~keyword:"pub" x) ||
           (potentially_conflicts_with ~keyword:"pri" x))
      -> string_add_suffix x
  | everything_else -> everything_else 
let escape_string str =
  let buf = Buffer.create (String.length str)  in
  String.iter
    (fun c  ->
       match c with
       | '\t' -> Buffer.add_string buf "\\t"
       | '\r' -> Buffer.add_string buf "\\r"
       | '\n' -> Buffer.add_string buf "\\n"
       | '\\' -> Buffer.add_string buf "\\\\"
       | '"' -> Buffer.add_string buf "\\\""
       | c when c < ' ' -> Buffer.add_string buf (Char.escaped c)
       | c -> Buffer.add_char buf c) str;
  Buffer.contents buf 
module TrailingCommaMarker =
  struct let char = Char.chr 249 
         let string = String.make 1 char  end
module OpenBraceMarker =
  struct let char = Char.chr 174 
         let string = String.make 1 char  end
module ClosedBraceMarker =
  struct let char = Char.chr 175 
         let string = String.make 1 char  end
let is_prefixed prefix str i =
  let len = String.length prefix  in
  let j = ref 0  in
  while
    ((!j) < len) &&
      ((String.unsafe_get prefix (!j)) = (String.unsafe_get str (i + (!j))))
    do incr j done;
  (!j) = len
  [@@ocaml.doc
    " [is_prefixed prefix i str] checks if prefix is the prefix of str\n  * starting from position i\n  "]
let rec pick_while p =
  function
  | [] -> ([], [])
  | hd::tl when p hd ->
      let (satisfied,not_satisfied) = pick_while p tl  in
      ((hd :: satisfied), not_satisfied)
  | l -> ([], l)
  [@@ocaml.doc
    "\n * pick_while returns a tuple where first element is longest prefix (possibly empty) of the list of elements that satisfy p\n * and second element is the remainder of the list\n "]
let find_substring sub str i =
  let len = (String.length str) - (String.length sub)  in
  let found = ref false
  
  and i = ref i
   in
  while (not (!found)) && ((!i) <= len) do
    if is_prefixed sub str (!i) then found := true else incr i done;
  if not (!found) then raise Not_found;
  !i
  [@@ocaml.doc
    " [find_substring sub str i]\n    returns the smallest [j >= i] such that [sub = str.[j..length sub - 1]]\n    raises [Not_found] if there is no such j\n    behavior is not defined if [sub] is the empty string\n"]
let replace_string old_str new_str str =
  match find_substring old_str str 0 with
  | exception Not_found  -> str
  | occurrence ->
      let buffer = Buffer.create ((String.length str) + 15)  in
      let rec loop i j =
        Buffer.add_substring buffer str i (j - i);
        Buffer.add_string buffer new_str;
        (let i = j + (String.length old_str)  in
         match find_substring old_str str i with
         | j -> loop i j
         | exception Not_found  ->
             Buffer.add_substring buffer str i ((String.length str) - i))
         in
      (loop 0 occurrence; Buffer.contents buffer)
  [@@ocaml.doc
    " [replace_string old_str new_str str] replaces old_str to new_str in str "]
let split_by ?(keep_empty= false)  is_delim str =
  let len = String.length str  in
  let rec loop acc last_pos pos =
    if pos = (-1)
    then
      (if (last_pos = 0) && (not keep_empty)
       then acc
       else (String.sub str 0 last_pos) :: acc)
    else
      if is_delim (str.[pos])
      then
        (let new_len = (last_pos - pos) - 1  in
         if (new_len <> 0) || keep_empty
         then
           let v = String.sub str (pos + 1) new_len  in
           loop (v :: acc) pos (pos - 1)
         else loop acc pos (pos - 1))
      else loop acc last_pos (pos - 1)
     in
  loop [] len (len - 1) 
let rec trim_right_idx str idx =
  if idx = (-1)
  then 0
  else
    (match str.[idx] with
     | '\t'|' '|'\n'|'\r' -> trim_right_idx str (idx - 1)
     | _ -> idx + 1)
  
let trim_right str =
  let length = String.length str  in
  if length = 0
  then ""
  else
    (let index = trim_right_idx str (length - 1)  in
     if index = 0
     then ""
     else if index = length then str else String.sub str 0 index)
  
let processLine line =
  let rightTrimmed = trim_right line  in
  let trimmedLen = String.length rightTrimmed  in
  if trimmedLen = 0
  then rightTrimmed
  else
    (let segments =
       split_by ~keep_empty:false (fun c  -> c = TrailingCommaMarker.char)
         rightTrimmed
        in
     let hadTrailingCommaMarkerBeforeNewline =
       (rightTrimmed.[trimmedLen - 1]) = TrailingCommaMarker.char  in
     let almostEverything = String.concat "" segments  in
     if hadTrailingCommaMarkerBeforeNewline
     then almostEverything ^ ","
     else almostEverything)
  
let processLineEndingsAndStarts str =
  (((split_by ~keep_empty:true (fun x  -> x = '\n') str) |>
      (List.map processLine))
     |> (String.concat "\n"))
    |> String.trim
  
let syntax_error_extension_node loc message =
  let str = Location.mkloc "merlin.syntax-error" loc  in
  let payload =
    PStr
      [{
         pstr_loc = Location.none;
         pstr_desc =
           (Pstr_eval
              ({
                 pexp_loc = Location.none;
                 pexp_desc =
                   (Pexp_constant (Parsetree.Pconst_string (message, None)));
                 pexp_attributes = []
               }, []))
       }]
     in
  (str, payload)
  [@@ocaml.doc
    " Generate a suitable extension node for Merlin's consumption,\n    for the purposes of reporting a syntax error - only used\n    in recovery mode.\n "]
let identifier_mapper f super =
  {
    super with
    expr =
      (fun mapper  ->
         fun expr  ->
           let expr =
             match expr with
             | { pexp_desc = Pexp_ident ({ txt } as id); pexp_loc;
                 pexp_attributes } ->
                 let swapped =
                   match txt with
                   | Lident s -> Lident (f s)
                   | Ldot (longPrefix,s) -> Ldot (longPrefix, (f s))
                   | Lapply (y,s) -> Lapply (y, s)  in
                 {
                   expr with
                   pexp_desc = (Pexp_ident { id with txt = swapped })
                 }
             | _ -> expr  in
           super.expr mapper expr);
    pat =
      (fun mapper  ->
         fun pat  ->
           let pat =
             match pat with
             | { ppat_desc = Ppat_var ({ txt } as id); ppat_loc;
                 ppat_attributes } ->
                 { pat with ppat_desc = (Ppat_var { id with txt = (f txt) })
                 }
             | _ -> pat  in
           super.pat mapper pat);
    signature_item =
      (fun mapper  ->
         fun signatureItem  ->
           let signatureItem =
             match signatureItem with
             | { psig_desc = Psig_value ({ pval_name } as name); psig_loc }
                 ->
                 {
                   signatureItem with
                   psig_desc =
                     (Psig_value
                        {
                          name with
                          pval_name =
                            { pval_name with txt = (f (name.pval_name).txt) }
                        })
                 }
             | _ -> signatureItem  in
           super.signature_item mapper signatureItem)
  }
  [@@ocaml.doc
    " identifier_mapper maps all identifiers in an AST with a mapping function f\n  this is used by swap_operator_mapper right below, to traverse the whole AST\n  and swapping the symbols listed above.\n  "]
let escape_stars_slashes_mapper =
  let escape_stars_slashes str =
    if String.contains str '/'
    then
      (replace_string "/*" "/\\*") @@
        ((replace_string "*/" "*\\/") @@
           ((replace_string "//" "/\\/") @@ str))
    else str  in
  identifier_mapper escape_stars_slashes
  [@@ocaml.doc
    " escape_stars_slashes_mapper escapes all stars and slases in an AST "]
let reason_to_ml_swap_operator_mapper = identifier_mapper reason_to_ml_swap 
let ml_to_reason_swap_operator_mapper = identifier_mapper ml_to_reason_swap 
let attribute_equals to_compare = function | ({ txt;_},_) -> txt = to_compare 
let attribute_exists txt attributes =
  List.exists (attribute_equals txt) attributes 
let attributes_conflicted attribute1 attribute2 attributes =
  (attribute_exists attribute1 attributes) &&
    (attribute_exists attribute2 attributes)
  
let normalized_attributes attribute attributes =
  List.filter (fun x  -> not (attribute_equals attribute x)) attributes 
let apply_mapper_to_structure s mapper = mapper.structure mapper s 
let apply_mapper_to_signature s mapper = mapper.signature mapper s 
let apply_mapper_to_type s mapper = mapper.typ mapper s 
let apply_mapper_to_expr s mapper = mapper.expr mapper s 
let apply_mapper_to_pattern s mapper = mapper.pat mapper s 
let apply_mapper_to_toplevel_phrase toplevel_phrase mapper =
  match toplevel_phrase with
  | Ptop_def x -> Ptop_def (apply_mapper_to_structure x mapper)
  | x -> x 
let apply_mapper_to_use_file use_file mapper =
  List.map (fun x  -> apply_mapper_to_toplevel_phrase x mapper) use_file 
type error =
  | Syntax_error of string 
exception Error of Location.t * error 
let report_error ppf (Syntax_error err) =
  let open Format in fprintf ppf "%s" err 
let () =
  Location.register_error_of_exn
    (function
     | Error (loc,err) ->
         Some (Location.error_of_printer loc report_error err)
     | _ -> None)
  
let map_first f =
  function
  | [] -> invalid_arg "Syntax_util.map_first: empty list"
  | x::xs -> (f x) :: xs 
let map_last f l =
  match List.rev l with
  | [] -> invalid_arg "Syntax_util.map_last: empty list"
  | x::xs -> List.rev ((f x) :: xs) 
type menhirMessagesError = {
  msg: string ;
  loc: Location.t }
type menhirError =
  | NoMenhirMessagesError 
  | MenhirMessagesError of menhirMessagesError 
let menhirMessagesError = ref [NoMenhirMessagesError] 
let findMenhirErrorMessage loc =
  let rec find messages =
    match messages with
    | (MenhirMessagesError err)::tail when err.loc = loc ->
        MenhirMessagesError err
    | _::tail -> find tail
    | [] -> NoMenhirMessagesError  in
  find (!menhirMessagesError) 
let default_error_message = "<syntax error>" 
let add_error_message err =
  let msg =
    try
      ignore (find_substring default_error_message err.msg 0);
      [MenhirMessagesError
         {
           err with
           msg =
             "A syntax error occurred. Help us improve this message: https://github.com/facebook/reason/blob/master/src/README.md#add-a-menhir-error-message"
         }]
    with | Not_found  -> [MenhirMessagesError err]  in
  menhirMessagesError := ((!menhirMessagesError) @ msg) 
let location_is_before loc1 loc2 =
  let open Location in
    (loc1.loc_end).Lexing.pos_cnum <= (loc2.loc_start).Lexing.pos_cnum
  
let location_contains loc1 loc2 =
  let open Location in
    ((loc1.loc_start).Lexing.pos_cnum <= (loc2.loc_start).Lexing.pos_cnum) &&
      ((loc1.loc_end).Lexing.pos_cnum >= (loc2.loc_end).Lexing.pos_cnum)
  