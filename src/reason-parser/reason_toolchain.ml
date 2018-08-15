[@@@ocaml.text
  "\n * Provides a simple interface to the most common parsing entrypoints required\n * by editor/IDE toolchains, preprocessors, and pretty printers.\n *\n * The form of this entrypoint includes more than what the standard OCaml\n * toolchain (oprof/ocamldoc) expects, but is still compatible.\n *\n * [implementation_with_comments] and [interface_with_comments] includes\n * additional information (about comments) suitable for building pretty\n * printers, editor, IDE and VCS integration.\n *\n * The comments include the full text of the comment (typically in between the\n * \"(*\" and the \"*)\", as well as location information for that comment.\n *\n * WARNING: The \"end\" location is one greater than the actual final position!\n * (for both [associatedTextLoc] and [commentLoc]).\n *\n * Currently, the location information for comments is of the form:\n *\n *  (associatedTextLoc)\n *\n * But we should quickly change it to be of the form:\n *\n *  (associatedTextLoc, commentLoc)\n *\n * Where the [commentLoc] is the actual original location of the comment,\n * and the [associatedTextLoc] records the location in the file that the\n * comment is attached to. If [associatedTextLoc] and [commentLoc] are the\n * same, then the comment is \"free floating\" in that it only attaches to itself.\n * The [Reason] pretty printer will try its best to interleave those comments\n * in the containing list etc. But if [associatedTextLoc] expands beyond\n * the [commentLoc] it means the comment and the AST that is captured by\n * the [associatedTextLoc] are related - where \"related\" is something\n * this [reason_toolchain] decides (but in short it handles \"end of line\n * comments\"). Various pretty printers can decide how to preserve this\n * relatedness. Ideally, it would preserve end of line comments, but in the\n * short term, it might merely use that relatedness to correctly attach\n * end of line comments to the \"top\" of the AST node.\n *\n *    let lst = [\n *\n *    ];   (*    Comment    *)\n *         ----commentLoc-----\n *    ---associatedTextLoc----\n *\n *\n * Ideally that would be formatted as:\n *\n *    let lst = [\n *\n *    ];   (*    Comment    *)\n *\n * Or:\n *\n *    let lst = [ ];   (*    Comment    *)\n *\n *\n * But a shorter term solution would use that [associatedTextLoc] to at least\n * correctly attach the comment to the correct node, even if not \"end of line\".\n *\n *   (*    Comment    *)\n *   let lst = [ ];\n "]
open Migrate_parsetree
open Ast_404
open Location
open Lexing
module From_current = ((Convert)(OCaml_current))(OCaml_404)
module To_current = ((Convert)(OCaml_404))(OCaml_current)
module S = MenhirLib.General
module Comment = Reason_comment
let setup_lexbuf use_stdin filename =
  let lexbuf =
    match use_stdin with
    | true  -> Lexing.from_channel stdin
    | false  ->
        let file_chan = open_in filename  in
        (seek_in file_chan 0; Lexing.from_channel file_chan)
     in
  Location.init lexbuf filename; lexbuf 
module type Toolchain  =
  sig
    val core_type_with_comments :
      Lexing.lexbuf -> (Parsetree.core_type * Reason_comment.t list)
    val implementation_with_comments :
      Lexing.lexbuf -> (Parsetree.structure * Reason_comment.t list)
    val interface_with_comments :
      Lexing.lexbuf -> (Parsetree.signature * Reason_comment.t list)
    val core_type : Lexing.lexbuf -> Parsetree.core_type
    val implementation : Lexing.lexbuf -> Parsetree.structure
    val interface : Lexing.lexbuf -> Parsetree.signature
    val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
    val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
    val print_interface_with_comments :
      Format.formatter ->
        (Parsetree.signature * Reason_comment.t list) -> unit
    val print_implementation_with_comments :
      Format.formatter ->
        (Parsetree.structure * Reason_comment.t list) -> unit
  end
module type Toolchain_spec  =
  sig
    val safeguard_parsing :
      Lexing.lexbuf ->
        (unit -> ('a * Reason_comment.t list)) ->
          ('a * Reason_comment.t list)
    type token
    module Lexer_impl :
    sig
      val init : unit -> unit
      val token : Lexing.lexbuf -> token
      val comments : unit -> (String.t * Location.t) list
    end
    val core_type : Lexing.lexbuf -> Parsetree.core_type
    val implementation : Lexing.lexbuf -> Parsetree.structure
    val interface : Lexing.lexbuf -> Parsetree.signature
    val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
    val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
    val format_interface_with_comments :
      (Parsetree.signature * Reason_comment.t list) ->
        Format.formatter -> unit
    val format_implementation_with_comments :
      (Parsetree.structure * Reason_comment.t list) ->
        Format.formatter -> unit
  end
let rec left_expand_comment should_scan_prev_line source loc_start =
  if loc_start = 0
  then ((String.unsafe_get source 0), true, 0)
  else
    (let c = String.unsafe_get source (loc_start - 1)  in
     match c with
     | '\t'|' ' ->
         left_expand_comment should_scan_prev_line source (loc_start - 1)
     | '\n' when should_scan_prev_line ->
         left_expand_comment should_scan_prev_line source (loc_start - 1)
     | '\n' -> (c, true, loc_start)
     | _ -> (c, false, loc_start))
  
let rec right_expand_comment should_scan_next_line source loc_start =
  if loc_start = (String.length source)
  then
    ((String.unsafe_get source ((String.length source) - 1)), true,
      (String.length source))
  else
    (let c = String.unsafe_get source loc_start  in
     match c with
     | '\t'|' ' ->
         right_expand_comment should_scan_next_line source (loc_start + 1)
     | '\n' when should_scan_next_line ->
         right_expand_comment should_scan_next_line source (loc_start + 1)
     | '\n' -> (c, true, loc_start)
     | _ -> (c, false, loc_start))
  
module Create_parse_entrypoint(Toolchain_impl:Toolchain_spec) : Toolchain =
  struct
    let buffer_add_lexbuf buf skip lexbuf =
      let bytes = lexbuf.Lexing.lex_buffer  in
      let start = lexbuf.Lexing.lex_start_pos + skip  in
      let stop = lexbuf.Lexing.lex_buffer_len  in
      Buffer.add_subbytes buf bytes start (stop - start) 
    let refill_buff buf refill lb =
      let skip = lb.Lexing.lex_buffer_len - lb.Lexing.lex_start_pos  in
      let result = refill lb  in buffer_add_lexbuf buf skip lb; result 
    let keep_from_lexbuf buffer lexbuf =
      buffer_add_lexbuf buffer 0 lexbuf;
      (let refill_buff = refill_buff buffer lexbuf.Lexing.refill_buff  in
       { lexbuf with refill_buff })
      
    let wrap_with_comments parsing_fun lexbuf =
      let input_copy = Buffer.create 0  in
      let lexbuf = keep_from_lexbuf input_copy lexbuf  in
      Toolchain_impl.safeguard_parsing lexbuf
        (fun ()  ->
           let _ = Toolchain_impl.Lexer_impl.init ()  in
           let ast = parsing_fun lexbuf  in
           let unmodified_comments = Toolchain_impl.Lexer_impl.comments ()
              in
           let contents = Buffer.contents input_copy  in
           Buffer.reset input_copy;
           if contents = ""
           then
             (let _ = Parsing.clear_parser ()  in
              let make_regular (text,location) =
                Comment.make ~location Comment.Regular text  in
              (ast, (List.map make_regular unmodified_comments)))
           else
             (let rec classifyAndNormalizeComments unmodified_comments =
                match unmodified_comments with
                | [] -> []
                | hd::tl ->
                    let classifiedTail = classifyAndNormalizeComments tl  in
                    let (str,physical_loc) = hd  in
                    let (stop_char,eol_start,virtual_start_pos) =
                      left_expand_comment false contents
                        (physical_loc.loc_start).pos_cnum
                       in
                    let one_char_before_stop_char =
                      if virtual_start_pos <= 1
                      then ' '
                      else String.unsafe_get contents (virtual_start_pos - 2)
                       in
                    let should_scan_next_line =
                      (stop_char = '|') &&
                        ((one_char_before_stop_char = ' ') ||
                           ((one_char_before_stop_char = '\n') ||
                              (one_char_before_stop_char = '\t')))
                       in
                    let (stop_char,eol_end,virtual_end_pos) =
                      right_expand_comment should_scan_next_line contents
                        (physical_loc.loc_end).pos_cnum
                       in
                    let end_pos_plus_one = (physical_loc.loc_end).pos_cnum
                       in
                    let comment_length =
                      (end_pos_plus_one - (physical_loc.loc_start).pos_cnum)
                        - 4
                       in
                    let original_comment_contents =
                      String.sub contents
                        ((physical_loc.loc_start).pos_cnum + 2)
                        comment_length
                       in
                    let location =
                      {
                        physical_loc with
                        loc_start =
                          {
                            (physical_loc.loc_start) with
                            pos_cnum = virtual_start_pos
                          };
                        loc_end =
                          {
                            (physical_loc.loc_end) with
                            pos_cnum = virtual_end_pos
                          }
                      }  in
                    let just_after loc' =
                      ((loc'.loc_start).pos_cnum ==
                         ((location.loc_end).pos_cnum - 1))
                        &&
                        ((loc'.loc_start).pos_lnum ==
                           (location.loc_end).pos_lnum)
                       in
                    let category =
                      match (eol_start, eol_end, classifiedTail) with
                      | (true ,true ,_) -> Comment.SingleLine
                      | (false ,true ,_) -> Comment.EndOfLine
                      | (false ,false ,comment::_) when
                          ((Comment.category comment) = Comment.EndOfLine) &&
                            (just_after (Comment.location comment))
                          -> Comment.EndOfLine
                      | _ -> Comment.Regular  in
                    let comment =
                      Comment.make ~location category
                        original_comment_contents
                       in
                    comment :: classifiedTail
                 in
              let modified_and_comment_with_category =
                classifyAndNormalizeComments unmodified_comments  in
              let _ = Parsing.clear_parser ()  in
              (ast, modified_and_comment_with_category)))
      
    let invalidLex = "invalidCharacter.orComment.orString" 
    let implementation_with_comments lexbuf =
      try wrap_with_comments Toolchain_impl.implementation lexbuf
      with
      | err when !Reason_config.recoverable ->
          let (loc,msg) =
            match err with
            | Location.Error err -> ((err.loc), (err.msg))
            | _ ->
                let loc = Location.curr lexbuf  in
                (match Reason_syntax_util.findMenhirErrorMessage loc with
                 | Reason_syntax_util.MenhirMessagesError errMessage ->
                     ((errMessage.Reason_syntax_util.loc),
                       (errMessage.Reason_syntax_util.msg))
                 | _ -> (loc, invalidLex))
             in
          let error = Reason_syntax_util.syntax_error_extension_node loc msg
             in
          ([Ast_helper.Str.mk ~loc (Parsetree.Pstr_extension (error, []))],
            [])
      
    let core_type_with_comments lexbuf =
      try wrap_with_comments Toolchain_impl.core_type lexbuf
      with
      | err when !Reason_config.recoverable ->
          let (loc,msg) =
            match err with
            | Location.Error err -> ((err.loc), (err.msg))
            | _ -> ((Location.curr lexbuf), invalidLex)  in
          let error = Reason_syntax_util.syntax_error_extension_node loc msg
             in
          ((Ast_helper.Typ.mk ~loc (Parsetree.Ptyp_extension error)), [])
      
    let interface_with_comments lexbuf =
      try wrap_with_comments Toolchain_impl.interface lexbuf
      with
      | err when !Reason_config.recoverable ->
          let (loc,msg) =
            match err with
            | Location.Error err -> ((err.loc), (err.msg))
            | _ -> ((Location.curr lexbuf), invalidLex)  in
          let error = Reason_syntax_util.syntax_error_extension_node loc msg
             in
          ([Ast_helper.Sig.mk ~loc (Parsetree.Psig_extension (error, []))],
            [])
      
    let toplevel_phrase_with_comments lexbuf =
      wrap_with_comments Toolchain_impl.toplevel_phrase lexbuf 
    let use_file_with_comments lexbuf =
      wrap_with_comments Toolchain_impl.use_file lexbuf 
    let ast_only f lexbuf = (lexbuf |> f) |> fst
      [@@ocaml.doc
        " [ast_only] wraps a function to return only the ast component\n   "]
    let implementation = ast_only implementation_with_comments 
    let core_type = ast_only core_type_with_comments 
    let interface = ast_only interface_with_comments 
    let toplevel_phrase = ast_only toplevel_phrase_with_comments 
    let use_file = ast_only use_file_with_comments 
    let print_interface_with_comments formatter interface =
      Toolchain_impl.format_interface_with_comments interface formatter 
    let print_implementation_with_comments formatter implementation =
      Toolchain_impl.format_implementation_with_comments implementation
        formatter
      
  end 
module OCaml_syntax =
  struct
    open Migrate_parsetree
    let doc_comments_filter () =
      let open Ast_mapper in
        let open Parsetree in
          let seen = Hashtbl.create 7  in
          let attribute mapper =
            function
            | ({ Location.txt = ("ocaml.doc"|"ocaml.text") },PStr
               ({
                  pstr_desc = Pstr_eval
                    ({
                       pexp_desc = Pexp_constant (Pconst_string
                         (_text,None ));_},_);
                  pstr_loc = loc;_}::[]))
                as attribute ->
                (Hashtbl.add seen loc ();
                 default_mapper.attribute mapper attribute)
            | attribute -> default_mapper.attribute mapper attribute  in
          let mapper = { default_mapper with attribute }  in
          let filter (_text,loc) = not (Hashtbl.mem seen loc)  in
          (mapper, filter)
      
    module Lexer_impl =
      struct
        let init = Lexer.init 
        let token = Lexer.token 
        let filtered_comments = ref [] 
        let filter_comments filter =
          filtered_comments := (List.filter filter (Lexer.comments ())) 
        let comments () = !filtered_comments 
      end
    module OCaml_parser = Parser
    type token = OCaml_parser.token
    let parse_and_filter_doc_comments iter fn lexbuf =
      let (it,filter) = doc_comments_filter ()  in
      let result = fn lexbuf  in
      ignore (iter it result); Lexer_impl.filter_comments filter; result 
    let implementation lexbuf =
      parse_and_filter_doc_comments (fun it  -> it.Ast_mapper.structure it)
        (fun lexbuf  ->
           From_current.copy_structure
             (Parser.implementation Lexer.token lexbuf)) lexbuf
      
    let core_type lexbuf =
      parse_and_filter_doc_comments (fun it  -> it.Ast_mapper.typ it)
        (fun lexbuf  ->
           From_current.copy_core_type
             (Parser.parse_core_type Lexer.token lexbuf)) lexbuf
      
    let interface lexbuf =
      parse_and_filter_doc_comments (fun it  -> it.Ast_mapper.signature it)
        (fun lexbuf  ->
           From_current.copy_signature (Parser.interface Lexer.token lexbuf))
        lexbuf
      
    let filter_toplevel_phrase it =
      function
      | Parsetree.Ptop_def str -> ignore (it.Ast_mapper.structure it str)
      | Parsetree.Ptop_dir _ -> () 
    let toplevel_phrase lexbuf =
      parse_and_filter_doc_comments filter_toplevel_phrase
        (fun lexbuf  ->
           From_current.copy_toplevel_phrase
             (Parser.toplevel_phrase Lexer.token lexbuf)) lexbuf
      
    let use_file lexbuf =
      parse_and_filter_doc_comments
        (fun it  ->
           fun result  -> List.map (filter_toplevel_phrase it) result)
        (fun lexbuf  ->
           List.map From_current.copy_toplevel_phrase
             (Parser.use_file Lexer.token lexbuf)) lexbuf
      
    let rec skip_phrase lexbuf =
      try
        match Lexer.token lexbuf with
        | OCaml_parser.SEMISEMI |OCaml_parser.EOF  -> ()
        | _ -> skip_phrase lexbuf
      with
      | Lexer.Error (Lexer.Unterminated_comment _,_)|Lexer.Error
        (Lexer.Unterminated_string ,_)|Lexer.Error
        (Lexer.Unterminated_string_in_comment _,_)|Lexer.Error
        (Lexer.Illegal_character _,_) -> skip_phrase lexbuf
      
    let maybe_skip_phrase lexbuf =
      if
        (Parsing.is_current_lookahead OCaml_parser.SEMISEMI) ||
          (Parsing.is_current_lookahead OCaml_parser.EOF)
      then ()
      else skip_phrase lexbuf 
    let safeguard_parsing lexbuf fn =
      try fn ()
      with
      | Lexer.Error (Lexer.Illegal_character _,_) as err when
          (!Location.input_name) = "//toplevel//" ->
          (skip_phrase lexbuf; raise err)
      | Syntaxerr.Error _ as err when (!Location.input_name) = "//toplevel//"
          -> (maybe_skip_phrase lexbuf; raise err)
      | Parsing.Parse_error |Syntaxerr.Escape_error  ->
          let loc = Location.curr lexbuf  in
          (if (!Location.input_name) = "//toplevel//"
           then maybe_skip_phrase lexbuf;
           raise (Syntaxerr.Error (Syntaxerr.Other loc)))
      
    let format_interface_with_comments (signature,_) formatter =
      Pprintast.signature formatter (To_current.copy_signature signature) 
    let format_implementation_with_comments (structure,_) formatter =
      Pprintast.structure formatter (To_current.copy_structure structure) 
  end
let insert_completion_ident = (ref None : Lexing.position option ref) 
module Reason_syntax =
  struct
    module I = Reason_parser.MenhirInterpreter
    module Lexer_impl =
      struct
        include Reason_lexer
        let init () =
          init ?insert_completion_ident:(!insert_completion_ident) () 
      end
    type token = Reason_parser.token
    type tracking_supplier =
      {
      mutable last_token: (token * Lexing.position * Lexing.position) option ;
      get_token: unit -> (token * Lexing.position * Lexing.position) }
    let lexbuf_to_supplier lexbuf =
      let s = I.lexer_lexbuf_to_supplier Reason_lexer.token lexbuf  in
      let eof_met = ref false  in
      let get_token () =
        let (token,s,e) = s ()  in
        if token = Reason_parser.EOF
        then
          (if not (!eof_met)
           then let _ = eof_met := true  in (token, s, e)
           else
             raise (Syntaxerr.Error (Syntaxerr.Other (Location.curr lexbuf))))
        else (token, s, e)  in
      let last_token = None  in { last_token; get_token } 
    let read supplier =
      let t = supplier.get_token ()  in supplier.last_token <- (Some t); t 
    let last_token_loc supplier =
      match supplier.last_token with
      | Some (_,s,e) -> { loc_start = s; loc_end = e; loc_ghost = false }
      | None  -> assert false 
    let stack checkpoint =
      match checkpoint with
      | I.HandlingError env -> I.stack env
      | _ -> assert false 
    let state checkpoint =
      (match Lazy.force (stack checkpoint) with
       | S.Nil  -> 0
       | S.Cons (I.Element (s,_,_,_),_) -> I.number s : int)
      
    let rec normalize_checkpoint =
      function
      | I.Shifting _|I.AboutToReduce _ as checkpoint ->
          normalize_checkpoint (I.resume checkpoint)
      | checkpoint -> checkpoint 
    let offer_normalize checkpoint triple =
      normalize_checkpoint (I.offer checkpoint triple) 
    let try_inserting_semi_on =
      function
      | Reason_parser.LET |Reason_parser.TYPE |Reason_parser.MODULE 
        |Reason_parser.OPEN |Reason_parser.EXCEPTION |Reason_parser.INCLUDE 
        |Reason_parser.DOCSTRING _|Reason_parser.LIDENT _
        |Reason_parser.UIDENT _|Reason_parser.IF |Reason_parser.WHILE 
        |Reason_parser.FOR |Reason_parser.SWITCH |Reason_parser.TRY 
        |Reason_parser.ASSERT |Reason_parser.LAZY |Reason_parser.LBRACKETAT 
          -> true
      | _ -> false 
    let try_inserting_semi checkpoint ((_,pos,_) as triple) =
      match offer_normalize checkpoint (Reason_parser.SEMI, pos, pos) with
      | I.InputNeeded _ as checkpoint' ->
          Some (offer_normalize checkpoint' triple)
      | _ -> None 
    let offer_normalize checkpoint triple =
      match offer_normalize checkpoint triple with
      | I.HandlingError _ as error_checkpoint ->
          (match triple with
           | (token,startp,endp) when try_inserting_semi_on token ->
               (match try_inserting_semi checkpoint triple with
                | Some (I.InputNeeded _ as checkpoint') -> checkpoint'
                | Some _|None  -> error_checkpoint)
           | _ -> error_checkpoint)
      | checkpoint -> checkpoint 
    let commit_invalid_docstrings =
      function
      | [] -> ()
      | docstrings ->
          let process_invalid_docstring (text,loc) =
            Reason_lexer.add_invalid_docstring text loc  in
          List.iter process_invalid_docstring (List.rev docstrings)
      
    let rec handle_other supplier checkpoint =
      match checkpoint with
      | I.InputNeeded _ ->
          (match supplier.last_token with
           | None  -> assert false
           | Some triple ->
               let checkpoint_with_previous_token = I.offer checkpoint triple
                  in
               let checkpoint =
                 match I.shifts checkpoint_with_previous_token with
                 | None  -> checkpoint
                 | Some _ ->
                     normalize_checkpoint checkpoint_with_previous_token
                  in
               handle_inputs_needed supplier [([], checkpoint)])
      | I.HandlingError env when !Reason_config.recoverable ->
          let loc = last_token_loc supplier  in
          ((match Reason_syntax_util.findMenhirErrorMessage loc with
            | Reason_syntax_util.MenhirMessagesError err -> ()
            | Reason_syntax_util.NoMenhirMessagesError  ->
                let token =
                  match supplier.last_token with
                  | Some token -> token
                  | None  -> assert false  in
                let msg = Reason_parser_explain.message env token  in
                Reason_syntax_util.add_error_message
                  (let open Reason_syntax_util in { loc; msg }));
           (let checkpoint = I.resume checkpoint  in
            handle_other supplier checkpoint))
      | I.HandlingError env ->
          let loc = last_token_loc supplier  in
          let token =
            match supplier.last_token with
            | Some token -> token
            | None  -> assert false  in
          let state = I.current_state_number env  in
          let msg = Reason_parser_explain.message env token  in
          let msg_with_state = Printf.sprintf "%d: %s" state msg  in
          raise
            (Reason_syntax_util.Error
               (loc, (Reason_syntax_util.Syntax_error msg_with_state)))
      | I.Rejected  ->
          let loc = last_token_loc supplier  in
          raise (let open Syntaxerr in Error (Syntaxerr.Other loc))
      | I.Accepted v -> v
      | I.Shifting _|I.AboutToReduce _ ->
          handle_other supplier (normalize_checkpoint checkpoint)
    
    and handle_inputs_needed supplier checkpoints =
      match read supplier with
      | (Reason_parser.ES6_FUN ,_,_) as triple ->
          let process_checkpoint ((invalid_docstrings,checkpoint) as x) tl =
            match offer_normalize checkpoint triple with
            | I.HandlingError _ -> x :: tl
            | checkpoint' -> x :: (invalid_docstrings, checkpoint') :: tl  in
          handle_inputs_needed supplier
            (List.fold_right process_checkpoint checkpoints [])
      | (Reason_parser.DOCSTRING text,loc_start,loc_end) as triple ->
          let process_checkpoint (invalid_docstrings,checkpoint) =
            match offer_normalize checkpoint triple with
            | I.HandlingError _ ->
                let invalid_docstring =
                  (text, { Location.loc_ghost = false; loc_start; loc_end })
                   in
                ((invalid_docstring :: invalid_docstrings), checkpoint)
            | checkpoint' -> (invalid_docstrings, checkpoint')  in
          handle_inputs_needed supplier
            (List.map process_checkpoint checkpoints)
      | triple ->
          (match checkpoints with
           | [] -> assert false
           | (docstrings,checkpoint)::[] ->
               (match offer_normalize checkpoint triple with
                | I.InputNeeded _ as checkpoint' ->
                    handle_inputs_needed supplier [(docstrings, checkpoint')]
                | checkpoint ->
                    (commit_invalid_docstrings docstrings;
                     handle_other supplier checkpoint))
           | checkpoints ->
               let rec process_checkpoints inputs_needed others =
                 function
                 | (docstrings,checkpoint)::xs ->
                     (match offer_normalize checkpoint triple with
                      | I.Accepted _ as other ->
                          (commit_invalid_docstrings docstrings;
                           handle_other supplier other)
                      | I.InputNeeded _ as checkpoint' ->
                          process_checkpoints ((docstrings, checkpoint') ::
                            inputs_needed) others xs
                      | other ->
                          process_checkpoints inputs_needed
                            ((docstrings, other) :: others) xs)
                 | [] ->
                     (match List.rev inputs_needed with
                      | [] ->
                          (match List.rev others with
                           | (docstrings,checkpoint)::_ ->
                               (commit_invalid_docstrings docstrings;
                                handle_other supplier checkpoint)
                           | [] -> assert false)
                      | inputs_needed ->
                          handle_inputs_needed supplier inputs_needed)
                  in
               process_checkpoints [] [] checkpoints)
    
    let initial_run constructor lexbuf =
      let checkpoint = constructor lexbuf.lex_curr_p  in
      let supplier = lexbuf_to_supplier lexbuf  in
      match normalize_checkpoint checkpoint with
      | I.InputNeeded _ as checkpoint ->
          handle_inputs_needed supplier [([], checkpoint)]
      | other -> handle_other supplier other 
    let implementation lexbuf =
      initial_run Reason_parser.Incremental.implementation lexbuf 
    let interface lexbuf =
      initial_run Reason_parser.Incremental.interface lexbuf 
    let core_type lexbuf =
      initial_run Reason_parser.Incremental.parse_core_type lexbuf 
    let toplevel_phrase lexbuf =
      initial_run Reason_parser.Incremental.toplevel_phrase lexbuf 
    let use_file lexbuf =
      initial_run Reason_parser.Incremental.use_file lexbuf 
    let rec skip_phrase lexbuf =
      try
        match Lexer_impl.token lexbuf with
        | Reason_parser.SEMI |Reason_parser.EOF  -> ()
        | _ -> skip_phrase lexbuf
      with
      | Lexer_impl.Error (Lexer_impl.Unterminated_comment _,_)
        |Lexer_impl.Error (Lexer_impl.Unterminated_string ,_)
        |Lexer_impl.Error (Lexer_impl.Unterminated_string_in_comment _,_)
        |Lexer_impl.Error (Lexer_impl.Illegal_character _,_) ->
          skip_phrase lexbuf
      
    let maybe_skip_phrase lexbuf =
      if
        (Parsing.is_current_lookahead Reason_parser.SEMI) ||
          (Parsing.is_current_lookahead Reason_parser.EOF)
      then ()
      else skip_phrase lexbuf 
    let safeguard_parsing lexbuf fn =
      try fn ()
      with
      | Lexer_impl.Error (Lexer_impl.Illegal_character _,_) as err when
          (!Location.input_name) = "//toplevel//" ->
          (skip_phrase lexbuf; raise err)
      | Syntaxerr.Error _ as err when (!Location.input_name) = "//toplevel//"
          -> (maybe_skip_phrase lexbuf; raise err)
      | Parsing.Parse_error |Syntaxerr.Escape_error  ->
          let loc = Location.curr lexbuf  in
          (if (!Location.input_name) = "//toplevel//"
           then maybe_skip_phrase lexbuf;
           raise (Syntaxerr.Error (Syntaxerr.Other loc)))
      | Error _ as x ->
          let loc = Location.curr lexbuf  in
          if (!Location.input_name) = "//toplevel//"
          then
            let _ = maybe_skip_phrase lexbuf  in
            raise (Syntaxerr.Error (Syntaxerr.Other loc))
          else raise x
      | x -> raise x 
    let format_interface_with_comments (signature,comments) formatter =
      let reason_formatter = Reason_pprint_ast.createFormatter ()  in
      reason_formatter#signature comments formatter signature 
    let format_implementation_with_comments (implementation,comments)
      formatter =
      let reason_formatter = Reason_pprint_ast.createFormatter ()  in
      reason_formatter#structure comments formatter implementation 
  end
module ML = (Create_parse_entrypoint)(OCaml_syntax)
module RE = (Create_parse_entrypoint)(Reason_syntax)