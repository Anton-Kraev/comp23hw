open Angstrom
open Ast

let apply p s = Angstrom.parse_string p s

(* helper functions and parsers *)
module Expression = struct
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

  let is_ignored = function
    | '\x20' | '\x09' | '\x0d' | '\x0a' | '\x0c' -> true
    | _ -> false
  ;;

  let ignored = skip_while is_ignored
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
  ;;

  let rec chainl1 parser op_parser =
    let rec loop acc =
      op_parser >>= fun op -> parser >>= fun x -> loop (op acc x) <|> return acc
    in
    parser >>= loop
  ;;

  let rec expression_p input =
    let literal_p =
      spaces
      *>
      let int_literal_p = integer >>| fun x -> LInt x in
      let bool_literal_p =
        string "true" <|> string "false" >>| bool_of_string >>| fun x -> LBool x
      in
      let parse_literal_p = choice [ int_literal_p; bool_literal_p ] in
      lift e_literal parse_literal_p
    in
    let identifier_p = fail "TODO" in
    let unary_op_p =
      spaces *> lift2 e_unary_op (char '~' >>| uneg) (expression_p input)
      <|> lift2 e_unary_op (string "not" >>| unot) (expression_p input)
    in
    let binary_op_p =
      spaces
      *>
      let multiplicative = spaces *> choice [ char '*' >>| bmul; char '/' >>| bdiv ] in
      let additive = spaces *> choice [ char '+' >>| badd; char '-' >>| bsub ] in
      let relational =
        spaces
        *> choice
             [ string ">=" >>| bgte
             ; string "<=" >>| blte
             ; char '>' >>| bgt
             ; char '<' >>| blt
             ]
      in
      let equality = spaces *> string "=" >>| beq in
      let logical_and = spaces *> string "andalso" >>| band in
      let logical_or = spaces *> string "orelse" >>| bor in
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
        (expression_p input)
        [ logical_or; logical_and; equality; relational; additive; multiplicative ]
    in
    let app_p =
      lift2 (fun e1 e2 -> EApp (e1, e2)) (expression_p input) (expression_p input)
    in
    let abs_p =
      lift2 (fun x e -> EAbs (x, e)) (string "=>" *> identifier_p) (expression_p input)
    in
    let if_then_else_p =
      spaces *> string "if" *> expression_p input
      <* string "then"
      >>= fun cond_expr ->
      expression_p input
      <* string "else"
      >>= fun then_expr ->
      expression_p input
      >>= fun else_expr -> return (EIfThenElse (cond_expr, then_expr, else_expr))
    in
    let let_in_p = fail "TODO" in
    choice
      [ literal_p
      ; identifier_p
      ; unary_op_p
      ; binary_op_p
      ; app_p
      ; abs_p
      ; if_then_else_p
      ; let_in_p
      ]
  ;;
end
