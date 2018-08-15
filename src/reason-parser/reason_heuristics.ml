let is_punned_labelled_expression e lbl =
  let open Ast_404.Parsetree in
    match e.pexp_desc with
    | Pexp_ident { txt;_}|Pexp_constraint
      ({ pexp_desc = Pexp_ident { txt;_};_},_)|Pexp_coerce
      ({ pexp_desc = Pexp_ident { txt;_};_},_,_) ->
        txt = (Longident.parse lbl)
    | _ -> false
  
let funAppCallbackExceedsWidth ~printWidth  ~args  ~funExpr  () =
  let open Ast_404.Parsetree in
    let open Ast_404.Asttypes in
      let funLen =
        match funExpr.pexp_desc with
        | Pexp_ident ident ->
            let identList = Longident.flatten ident.txt  in
            let lengthOfDots = (List.length identList) - 1  in
            let len =
              List.fold_left
                (fun acc  -> fun curr  -> acc + (String.length curr))
                lengthOfDots identList
               in
            len
        | _ -> (-1)  in
      let rec aux len =
        function
        | _ when len < 0 -> true
        | [] -> false
        | arg::args ->
            (match arg with
             | (label,({ pexp_desc = Pexp_ident ident } as e)) ->
                 let identLen =
                   List.fold_left
                     (fun acc  -> fun curr  -> acc + (String.length curr))
                     len (Longident.flatten ident.txt)
                    in
                 (match label with
                  | Nolabel  -> aux (len - identLen) args
                  | Labelled s when is_punned_labelled_expression e s ->
                      aux (len - (identLen + 1)) args
                  | Labelled s ->
                      aux (len - ((identLen + 2) + (String.length s))) args
                  | Optional s ->
                      aux (len - ((identLen + 3) + (String.length s))) args)
             | (label,{ pexp_desc = Pexp_constant (Pconst_string (str,_)) })
                 ->
                 let strLen = String.length str  in
                 (match label with
                  | Nolabel  -> aux (len - strLen) args
                  | Labelled s ->
                      aux (len - ((strLen + 2) + (String.length s))) args
                  | Optional s ->
                      aux (len - ((strLen + 3) + (String.length s))) args)
             | _ -> true)
         in
      aux (printWidth - funLen) args
  
let singleTokenPatternOmmitTrail txt = (String.length txt) < 4 
let bsExprCanBeUncurried expr =
  match let open Ast_404.Parsetree in expr.pexp_desc with
  | Pexp_fun _|Pexp_apply _ -> true
  | _ -> false 
let isUnderscoreIdent expr =
  match let open Ast_404.Parsetree in expr.pexp_desc with
  | Pexp_ident { txt = Lident "_" } -> true
  | _ -> false 
let isFastPipe e =
  match let open Ast_404.Parsetree in e.pexp_desc with
  | Pexp_ident { txt = Longident.Lident "|." } -> true
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Longident.Lident "|." } },_)
      -> true
  | _ -> false 
let isFastPipeWithApplicationJSXChild e =
  match let open Ast_404.Parsetree in e.pexp_desc with
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Longident.Lident "|." } },(Nolabel
                                                                   ,{
                                                                    pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    _ })::_::[])
      -> true
  | _ -> false 
let isUnderscoreApplication expr =
  let open Ast_404.Parsetree in
    match expr with
    | { pexp_attributes = [];
        pexp_desc = Pexp_fun
          (Nolabel ,None
           ,{ ppat_desc = Ppat_var { txt = "__x" }; ppat_attributes = [] },_)
        } -> true
    | _ -> false
  