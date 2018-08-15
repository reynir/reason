open Migrate_parsetree
open Ast_404
module To_current = ((Convert)(OCaml_404))(OCaml_current)
let nolabel = Ast_404.Asttypes.Nolabel 
let labelled str = Ast_404.Asttypes.Labelled str 
let argIsKeyRef =
  function
  | (Asttypes.Labelled ("key"|"ref"),_)|(Asttypes.Optional ("key"|"ref"),_)
      -> true
  | _ -> false 
let constantString ~loc  str =
  Ast_helper.Exp.constant ~loc (Parsetree.Pconst_string (str, None)) 
open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
let transformChildrenIfList ~loc  ~mapper  theList =
  let rec transformChildren_ theList accum =
    match theList with
    | { pexp_desc = Pexp_construct ({ txt = Lident "[]" },None ) } ->
        (List.rev accum) |> (Exp.array ~loc)
    | {
        pexp_desc = Pexp_construct
          ({ txt = Lident "::" },Some
           { pexp_desc = Pexp_tuple (v::acc::[]) })
        } -> transformChildren_ acc ((mapper.expr mapper v) :: accum)
    | notAList -> mapper.expr mapper notAList  in
  transformChildren_ theList [] 
let extractChildren ?(removeLastPositionUnit= false)  ~loc  propsAndChildren
  =
  let rec allButLast_ lst acc =
    match lst with
    | [] -> []
    | (Nolabel ,{ pexp_desc = Pexp_construct ({ txt = Lident "()" },None ) })::[]
        -> acc
    | (Nolabel ,_)::rest ->
        raise
          (Invalid_argument
             "JSX: found non-labelled argument before the last position")
    | arg::rest -> allButLast_ rest (arg :: acc)  in
  let allButLast lst = (allButLast_ lst []) |> List.rev  in
  match List.partition (fun (label,expr)  -> label = (labelled "children"))
          propsAndChildren
  with
  | ([],props) ->
      ((Exp.construct ~loc { loc; txt = (Lident "[]") } None),
        (if removeLastPositionUnit then allButLast props else props))
  | ((label,childrenExpr)::[],props) ->
      (childrenExpr,
        (if removeLastPositionUnit then allButLast props else props))
  | (moreThanOneChild,props) ->
      raise
        (Invalid_argument
           "JSX: somehow there's more than one `children` label")
  
let jsxMapper () =
  let jsxVersion = ref None  in
  let transformUppercaseCall modulePath mapper loc attrs callExpression
    callArguments =
    let (children,argsWithLabels) =
      extractChildren ~loc ~removeLastPositionUnit:true callArguments  in
    let (argsKeyRef,argsForMake) = List.partition argIsKeyRef argsWithLabels
       in
    let childrenExpr = transformChildrenIfList ~loc ~mapper children  in
    let recursivelyTransformedArgsForMake =
      argsForMake |>
        (List.map
           (fun (label,expression)  ->
              (label, (mapper.expr mapper expression))))
       in
    let args = recursivelyTransformedArgsForMake @ [(nolabel, childrenExpr)]
       in
    let wrapWithReasonReactElement e =
      Exp.apply ~loc
        (Exp.ident ~loc
           { loc; txt = (Ldot ((Lident "ReasonReact"), "element")) })
        (argsKeyRef @ [(nolabel, e)])
       in
    (Exp.apply ~loc ~attrs
       (Exp.ident ~loc { loc; txt = (Ldot (modulePath, "make")) }) args)
      |> wrapWithReasonReactElement
     in
  let transformLowercaseCall mapper loc attrs callArguments id =
    let (children,nonChildrenProps) = extractChildren ~loc callArguments  in
    let componentNameExpr = constantString ~loc id  in
    let childrenExpr = transformChildrenIfList ~loc ~mapper children  in
    let createElementCall =
      match children with
      | {
          pexp_desc =
            (Pexp_construct
             ({ txt = Lident "::"; loc },Some { pexp_desc = Pexp_tuple _ })
             |Pexp_construct ({ txt = Lident "[]"; loc },None ));
          pexp_attributes } -> "createElement"
      | { pexp_desc = Pexp_array _; pexp_attributes } ->
          raise
            (Invalid_argument
               "A spread + an array literal as a DOM element's children would cancel each other out, and thus don't make sense written together. You can simply remove the spread and the array literal.")
      | { pexp_attributes } when
          pexp_attributes |>
            (List.exists (fun (attribute,_)  -> attribute.txt = "JSX"))
          ->
          raise
            (Invalid_argument
               "A spread + a JSX literal as a DOM element's children don't make sense written together. You can simply remove the spread.")
      | notAList -> "createElementVariadic"  in
    let args =
      match nonChildrenProps with
      | _justTheUnitArgumentAtEnd::[] ->
          [(nolabel, componentNameExpr); (nolabel, childrenExpr)]
      | nonEmptyProps ->
          let propsCall =
            Exp.apply ~loc
              (Exp.ident ~loc
                 { loc; txt = (Ldot ((Lident "ReactDOMRe"), "props")) })
              (nonEmptyProps |>
                 (List.map
                    (fun (label,expression)  ->
                       (label, (mapper.expr mapper expression)))))
             in
          [(nolabel, componentNameExpr);
          ((labelled "props"), propsCall);
          (nolabel, childrenExpr)]
       in
    Exp.apply ~loc ~attrs
      (Exp.ident ~loc
         { loc; txt = (Ldot ((Lident "ReactDOMRe"), createElementCall)) })
      args
     in
  let transformJsxCall mapper callExpression callArguments attrs =
    match callExpression.pexp_desc with
    | Pexp_ident caller ->
        (match caller with
         | { txt = Lident "createElement" } ->
             raise
               (Invalid_argument
                  "JSX: `createElement` should be preceeded by a module name.")
         | { loc; txt = Ldot (modulePath,("createElement"|"make")) } ->
             (match !jsxVersion with
              | None |Some 2 ->
                  transformUppercaseCall modulePath mapper loc attrs
                    callExpression callArguments
              | Some _ ->
                  raise (Invalid_argument "JSX: the JSX version must be 2"))
         | { loc; txt = Lident id } ->
             transformLowercaseCall mapper loc attrs callArguments id
         | { txt = Ldot (_,anythingNotCreateElementOrMake) } ->
             raise
               (Invalid_argument
                  ("JSX: the JSX attribute should be attached to a `YourModuleName.createElement` or `YourModuleName.make` call. We saw `"
                     ^ (anythingNotCreateElementOrMake ^ "` instead")))
         | { txt = Lapply _ } ->
             raise
               (Invalid_argument
                  "JSX: encountered a weird case while processing the code. Please report this!"))
    | anythingElseThanIdent ->
        raise
          (Invalid_argument
             "JSX: `createElement` should be preceeded by a simple, direct module name.")
     in
  let structure mapper structure =
    match structure with
    | { pstr_loc;
        pstr_desc = Pstr_attribute
          (({ txt = "bs.config" } as bsConfigLabel),PStr
           (({
               pstr_desc = Pstr_eval
                 (({ pexp_desc = Pexp_record (recordFields,b) } as
                     innerConfigRecord),a)
               } as configRecord)::[]))
        }::restOfStructure ->
        let (jsxField,recordFieldsWithoutJsx) =
          recordFields |>
            (List.partition (fun ({ txt },_)  -> txt = (Lident "jsx")))
           in
        (match (jsxField, recordFieldsWithoutJsx) with
         | ([],_) -> default_mapper.structure mapper structure
         | ((_,{ pexp_desc = Pexp_constant (Pconst_integer (version,_)) })::rest,recordFieldsWithoutJsx)
             ->
             ((match version with
               | "2" -> jsxVersion := (Some 2)
               | _ ->
                   raise
                     (Invalid_argument
                        "JSX: the file-level bs.config's jsx version must be 2"));
              (match recordFieldsWithoutJsx with
               | [] -> default_mapper.structure mapper restOfStructure
               | fields ->
                   default_mapper.structure mapper
                     ({
                        pstr_loc;
                        pstr_desc =
                          (Pstr_attribute
                             (bsConfigLabel,
                               (PStr
                                  [{
                                     configRecord with
                                     pstr_desc =
                                       (Pstr_eval
                                          ({
                                             innerConfigRecord with
                                             pexp_desc =
                                               (Pexp_record (fields, b))
                                           }, a))
                                   }])))
                      } :: restOfStructure)))
         | (_,recordFieldsWithoutJsx) ->
             raise
               (Invalid_argument
                  "JSX: the file-level bs.config's {jsx: ...} config accepts only a version number"))
    | _ -> default_mapper.structure mapper structure  in
  let expr mapper expression =
    match expression with
    | { pexp_desc = Pexp_apply (callExpression,callArguments);
        pexp_attributes } ->
        let (jsxAttribute,nonJSXAttributes) =
          List.partition (fun (attribute,_)  -> attribute.txt = "JSX")
            pexp_attributes
           in
        (match (jsxAttribute, nonJSXAttributes) with
         | ([],_) -> default_mapper.expr mapper expression
         | (_,nonJSXAttributes) ->
             transformJsxCall mapper callExpression callArguments
               nonJSXAttributes)
    | {
        pexp_desc =
          (Pexp_construct
           ({ txt = Lident "::"; loc },Some { pexp_desc = Pexp_tuple _ })
           |Pexp_construct ({ txt = Lident "[]"; loc },None ));
        pexp_attributes } as listItems ->
        let (jsxAttribute,nonJSXAttributes) =
          List.partition (fun (attribute,_)  -> attribute.txt = "JSX")
            pexp_attributes
           in
        (match (jsxAttribute, nonJSXAttributes) with
         | ([],_) -> default_mapper.expr mapper expression
         | (_,nonJSXAttributes) ->
             let fragment =
               Exp.ident ~loc
                 { loc; txt = (Ldot ((Lident "ReasonReact"), "fragment")) }
                in
             let childrenExpr =
               transformChildrenIfList ~loc ~mapper listItems  in
             let args = [(nolabel, fragment); (nolabel, childrenExpr)]  in
             Exp.apply ~loc ~attrs:nonJSXAttributes
               (Exp.ident ~loc
                  {
                    loc;
                    txt = (Ldot ((Lident "ReactDOMRe"), "createElement"))
                  }) args)
    | e -> default_mapper.expr mapper e  in
  To_current.copy_mapper { default_mapper with structure; expr } 
let () = Compiler_libs.Ast_mapper.register "JSX" (fun _argv  -> jsxMapper ()) 