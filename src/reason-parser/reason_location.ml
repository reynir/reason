module Comment = Reason_comment
module Range =
  struct
    type t = {
      lnum_start: int ;
      lnum_end: int }[@@ocaml.doc
                       " [t] represents an interval, including endpoints,\n   * delimited by two linenumbers. "]
    let makeRangeBetween loc1 loc2 =
      let open Location in
        {
          lnum_start = ((loc1.loc_end).pos_lnum + 1);
          lnum_end = ((loc2.loc_start).pos_lnum - 1)
        }
      [@@ocaml.doc
        "\n   * make a range delimited by [loc1] and [loc2]\n   * 1| let a = 1;\n   * 2|\n   * 3|\n   * 4|\n   * 5| let b = 2;\n   * If loc1 represents `let a = 1` and loc2 represents `let b = 2`,\n   * we get the range: {lnum_start: 2; lnum_end 4}\n   "]
    let containsLoc range loc =
      let open Location in
        (range.lnum_start <= (loc.loc_start).pos_lnum) &&
          (range.lnum_end >= (loc.loc_end).pos_lnum)
      [@@ocaml.doc " check whether [range] contains the [loc] "]
    let containsWhitespace ?comments  ~range  () =
      let h =
        match comments with
        | Some comments ->
            List.fold_left
              (fun acc  ->
                 fun (curr : Comment.t)  ->
                   let cl = Comment.location curr  in
                   let open Location in
                     let startLnum = (cl.loc_start).pos_lnum  in
                     let endLnum = (cl.loc_end).pos_lnum  in
                     if containsLoc range cl
                     then acc + ((endLnum - startLnum) + 1)
                     else acc) 0 comments
        | None  -> 0  in
      ((range.lnum_end - range.lnum_start) - h) >= 0
      [@@ocaml.doc
        "\n   * checks if [range] contains whitespace.\n   * When comments are passed, the computation\n   * takes the height of the comments into account.\n   *\n   * Example:\n   * 1| let a = 1;\n   * 2|\n   * 3| /* a multi-\n   * 4|   line comment */\n   * 5| let b = 1;\n   * The range (line 2 - line 4) has whitespace.\n   *\n   * 1| let a = 1;\n   * 2| /* a multi-\n   * 3|   line comment */\n   * 4| let b = 1;\n   * The range (line 2 - line 3) does not have whitespace.\n   "]
  end
let hasSpaceBetween loc1 loc2 =
  (let open Location in (loc1.loc_start).pos_lnum - (loc2.loc_end).pos_lnum)
    > 1
  [@@ocaml.doc
    " compute if there's space (one or more line) between [loc1] and [loc2] "]