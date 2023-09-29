open Angstrom
open Ast

(* helper functions and parsers *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_first_char = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_varname_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

let spaces = take_while is_space
let parens p = spaces *> char '(' *> spaces *> p <* spaces <* char ')'

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
;;

let varname =
  peek_char_fail
  >>= fun first ->
  if is_first_char first
  then take_while is_varname_char
  else fail "Parsing error: bad first symbol of id."
;;

let id_of_expr = function
  | EIdentifier x -> return x
  | _ -> fail "Unreachable"
;;

let id_list_of_expr = function
  | EIdentifier x -> return [ x ]
  | _ -> fail "Unreachable"
;;

(* dispatch for parsers *)
type dispatch =
  { unary_op_p : dispatch -> Ast.expr Angstrom.t
  ; binary_op_p : dispatch -> Ast.expr Angstrom.t
  ; let_p : dispatch -> Ast.expr Angstrom.t
  ; application_p : dispatch -> Ast.expr Angstrom.t
  ; abstraction_p : dispatch -> Ast.expr Angstrom.t
  ; val_dec_p : dispatch -> Ast.expr Angstrom.t
  ; val_rec_dec_p : dispatch -> Ast.expr Angstrom.t
  ; if_then_else_p : dispatch -> Ast.expr Angstrom.t
  ; expr_p : dispatch -> Ast.expr Angstrom.t
  }

(* expression parsers *)
let literal_p =
  fix
  @@ fun self ->
  spaces
  *>
  let int_literal_p = integer >>| fun x -> LInt x in
  let bool_literal_p =
    string "true" <|> string "false" >>| bool_of_string >>| fun x -> LBool x
  in
  let parse_literal = choice [ int_literal_p; bool_literal_p ] in
  parens self <|> lift e_literal parse_literal
;;

let identifier_p =
  fix
  @@ fun _ ->
  spaces
  *>
  let keywords =
    [ "let"
    ; "fun"
    ; "val"
    ; "rec"
    ; "if"
    ; "then"
    ; "else"
    ; "in"
    ; "fn"
    ; "true"
    ; "false"
    ; "not"
    ; "orelse"
    ; "andalso"
    ; "end"
    ]
  in
  let parse_identifier =
    varname
    >>= fun name ->
    if List.exists (fun x -> x = name) keywords
    then fail "Parsing error: keyword used."
    else return @@ e_var name
  in
  parse_identifier
;;

let unary_op_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let parse_content_neg =
    choice
      [ parens self
      ; literal_p
      ; identifier_p
      ; d.binary_op_p d
      ; d.let_p d
      ; d.application_p d
      ; d.abstraction_p d
      ; d.if_then_else_p d
      ]
  in
  let parse_content_not =
    choice
      [ parens self
      ; parens @@ d.binary_op_p d
      ; parens @@ d.let_p d
      ; parens @@ d.application_p d
      ; parens @@ d.abstraction_p d
      ; parens @@ d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self
  <|> lift2 e_unary_op (char '~' >>| uneg) parse_content_neg
  <|> lift2 e_unary_op (string "not" >>| unot) parse_content_not
;;

let binary_op_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let multiplicative = spaces *> choice [ char '*' >>| bmul; char '/' >>| bdiv ]
  and additive = spaces *> choice [ char '+' >>| badd; char '-' >>| bsub ]
  and relational =
    spaces
    *> choice
         [ string ">=" >>| bgte
         ; string "<=" >>| blte
         ; char '>' >>| bgt
         ; char '<' >>| blte
         ]
  and equality = spaces *> choice [ string "=" >>| beq ]
  and logical_and = spaces *> (string "andalso" >>| band)
  and logical_or = spaces *> (string "orelse" >>| bor) in
  let parse_content =
    choice
      [ parens self
      ; d.unary_op_p d
      ; d.let_p d
      ; d.application_p d
      ; d.abstraction_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  let rec parse_bin_op expr_parser op_parsers =
    let chainl1 expr_p op_p =
      let rec go acc =
        lift2 (fun f x -> e_binary_op f acc x) op_p expr_p >>= go <|> return acc
      in
      expr_p >>= fun init -> go init
    in
    match op_parsers with
    | [ op ] -> chainl1 expr_parser op
    | h :: t -> chainl1 (parse_bin_op expr_parser t) h
    | _ -> fail "Unreachable"
  in
  parse_bin_op
    parse_content
    [ logical_or; logical_and; equality; relational; additive; multiplicative ]
;;
