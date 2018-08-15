module Easy_format = Vendored_easy_format
module Comment = Reason_comment
module Range = Reason_location.Range
type break_criterion =
  | Never 
  | IfNeed 
  | Always 
  | Always_rec 
type separator =
  | NoSep 
  | Sep of string 
  | SepFinal of string * string 
module WhitespaceRegion =
  struct
    type t = {
      range: Range.t ;
      comments: Comment.t list ;
      newlines: int }
    let make ~range  ~newlines  () = { range; comments = []; newlines } 
    let newlines t = t.newlines 
    let range t = t.range 
    let comments t = t.comments 
    let addComment t comment =
      { t with comments = (comment :: (t.comments)) } 
    let modifyNewlines t newNewlines = { t with newlines = newNewlines } 
  end[@@ocaml.doc
       "\n * Module concerning info to correctly interleave whitespace above a layout node.\n "]
type t =
  | SourceMap of Location.t * t 
  | Sequence of config * t list 
  | Label of (Easy_format.t -> Easy_format.t -> Easy_format.t) * t * t 
  | Easy of Easy_format.t 
  | Whitespace of WhitespaceRegion.t * t [@@ocaml.doc
                                           "\n * These represent \"intent to format\" the AST, with some parts being annotated\n * with original source location. The benefit of tracking this in an\n * intermediate structure, is that we can then interleave comments throughout\n * the tree before generating the final representation. That prevents the\n * formatting code from having to thread comments everywhere.\n *\n * The final representation is rendered using Easy_format.\n "]
and config =
  {
  break: break_criterion ;
  wrap: (string * string) ;
  inline: (bool * bool) ;
  sep: separator ;
  indent: int ;
  sepLeft: bool ;
  preSpace: bool ;
  postSpace: bool ;
  pad: (bool * bool) ;
  listConfigIfCommentsInterleaved: (config -> config) option ;
  listConfigIfEolCommentsInterleaved: (config -> config) option }
let string_of_easy =
  function
  | Easy_format.Atom (s,_) -> s
  | Easy_format.List (_,_) -> "list"
  | Easy_format.Label (_,_) -> "label"
  | Easy_format.Custom _ -> "custom" 
let indent_more indent = "  " ^ indent 
let dump_easy ppf easy =
  let printf fmt = Format.fprintf ppf fmt  in
  let rec traverse indent =
    function
    | Easy_format.Atom (s,_) -> printf "%s Atom:'%s'\n" indent s
    | Easy_format.List ((opening,sep,closing,config),items) ->
        let break =
          match config.wrap_body with
          | `No_breaks -> "No_breaks"
          | `Wrap_atoms -> "Wrap_atoms"
          | `Never_wrap -> "Never_wrap"
          | `Force_breaks -> "Force_breaks"
          | `Force_breaks_rec -> "Force_breaks_rec"
          | `Always_wrap -> "Always_wrap"  in
        (printf "%s List: open %s close %s sep %s break %s \n" indent opening
           closing sep break;
         (let _ = List.map (traverse (indent_more indent)) items  in ()))
    | Easy_format.Label ((left,config),right) ->
        let break =
          match config.label_break with
          | `Never -> "Never"
          | `Always_rec -> "Always_rec"
          | `Auto -> "Auto"
          | `Always -> "Always"  in
        (printf "%s Label (break = %s): \n" indent break;
         printf "  %s left \n" indent;
         (let indent' = indent_more indent  in
          traverse indent' left;
          printf "  %s right \n" indent;
          traverse indent' right))
    | Easy_format.Custom _ -> printf "custom \n"  in
  traverse "" easy 
let dump ppf layout =
  let printf fmt = Format.fprintf ppf fmt  in
  let rec traverse indent =
    function
    | SourceMap (loc,layout) ->
        (printf "%s SourceMap [(%d:%d)-(%d:%d)]\n" indent
           (loc.loc_start).Lexing.pos_lnum
           ((loc.loc_start).Lexing.pos_cnum - (loc.loc_start).Lexing.pos_bol)
           (loc.loc_end).Lexing.pos_lnum
           ((loc.loc_end).Lexing.pos_cnum - (loc.loc_end).Lexing.pos_bol);
         traverse (indent_more indent) layout)
    | Sequence (config,layout_list) ->
        let break =
          match config.break with
          | Never  -> "Never"
          | IfNeed  -> "if need"
          | Always  -> "Always"
          | Always_rec  -> "Always_rec"  in
        let sep =
          match config.sep with
          | NoSep  -> "NoSep"
          | Sep s -> "Sep '" ^ (s ^ "'")
          | SepFinal (s,finalSep) ->
              "SepFinal ('" ^ (s ^ ("', '" ^ (finalSep ^ "')")))
           in
        (printf "%s Sequence of %d, sep: %s, stick_to_left: %s break: %s\n"
           indent (List.length layout_list) sep
           (string_of_bool config.sepLeft) break;
         List.iter (traverse (indent_more indent)) layout_list)
    | Label (_,left,right) ->
        (printf "%s Label: \n" indent;
         printf "  %s left \n" indent;
         (let indent' = indent_more (indent_more indent)  in
          traverse indent' left;
          printf "  %s right \n" indent;
          traverse indent' right))
    | Easy e -> printf "%s Easy: '%s' \n" indent (string_of_easy e)
    | Whitespace (region,sublayout) ->
        let open WhitespaceRegion in
          (printf " %s Whitespace (%d) [%d %d]:\n" indent region.newlines
             (region.range).lnum_start (region.range).lnum_end;
           traverse (indent_more indent) sublayout)
     in
  traverse "" layout 
let source_map ?(loc= Location.none)  layout =
  if loc = Location.none then layout else SourceMap (loc, layout) 
let default_list_settings =
  {
    Easy_format.space_after_opening = false;
    space_after_separator = false;
    space_before_separator = false;
    separators_stick_left = true;
    space_before_closing = false;
    stick_to_label = true;
    align_closing = true;
    wrap_body = `No_breaks;
    indent_body = 0;
    list_style = (Some "list");
    opening_style = None;
    body_style = None;
    separator_style = None;
    closing_style = None
  } 
let easy_settings_from_config
  { break; wrap; inline; indent; sepLeft; preSpace; postSpace; pad; sep } =
  let (opn,cls) = wrap  in
  let (padOpn,padCls) = pad  in
  let (inlineStart,inlineEnd) = inline  in
  let sepStr = match sep with | NoSep  -> "" | Sep s|SepFinal (s,_) -> s  in
  (opn, sepStr, cls,
    {
      default_list_settings with
      Easy_format.wrap_body =
        (match break with
         | Never  -> `No_breaks
         | IfNeed  -> `Never_wrap
         | Always  -> `Force_breaks
         | Always_rec  -> `Force_breaks_rec);
      indent_body = indent;
      space_after_separator = postSpace;
      space_before_separator = preSpace;
      space_after_opening = padOpn;
      space_before_closing = padCls;
      stick_to_label = inlineStart;
      align_closing = (not inlineEnd)
    })
  
let to_easy_format layout =
  let rec traverse =
    function
    | Sequence (config,sublayouts) ->
        let items = List.map traverse sublayouts  in
        Easy_format.List ((easy_settings_from_config config), items)
    | Label (labelFormatter,left,right) ->
        labelFormatter (traverse left) (traverse right)
    | SourceMap (_,subLayout) -> traverse subLayout
    | Easy e -> e
    | Whitespace (_,subLayout) -> traverse subLayout  in
  traverse layout 
let get_location layout =
  let union loc1 loc2 =
    match (loc1, loc2) with
    | (None ,_) -> loc2
    | (_,None ) -> loc1
    | (Some loc1,Some loc2) ->
        Some { loc1 with Location.loc_end = (loc2.Location.loc_end) }
     in
  let rec traverse =
    function
    | Sequence (listConfig,subLayouts) ->
        let locs = List.map traverse subLayouts  in
        List.fold_left union None locs
    | Label (formatter,left,right) -> union (traverse left) (traverse right)
    | SourceMap (loc,_) -> Some loc
    | Whitespace (_,sub) -> traverse sub
    | _ -> None  in
  traverse layout
  [@@ocaml.doc
    " [getLocFromLayout] recursively takes the unioned location of its children,\n *  and returns the max one "]
let is_before ~location  layout =
  match get_location layout with
  | None  -> true
  | Some loc -> Reason_syntax_util.location_is_before loc location 
let contains_location layout ~location  =
  match get_location layout with
  | None  -> false
  | Some layout_loc ->
      Reason_syntax_util.location_contains layout_loc location
  