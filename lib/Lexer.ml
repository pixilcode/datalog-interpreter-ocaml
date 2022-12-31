module Token = struct
  type token_type =
    | Comma
    | Period
    | QMark
    | LeftParen
    | RightParen
    | ColonDash
    | Colon
    | Multiply
    | Add
    | Schemes
    | Facts
    | Rules
    | Queries
    | Id
    | String
    | Undefined
    | Eof

  let token_type_to_string t_type =
    match t_type with
    | Comma -> "COMMA"
    | Period -> "PERIOD"
    | QMark -> "Q_MARK"
    | LeftParen -> "LEFT_PAREN"
    | RightParen -> "RIGHT_PAREN"
    | ColonDash -> "COLON_DASH"
    | Colon -> "COLON"
    | Multiply -> "MULTIPLY"
    | Add -> "ADD"
    | Schemes -> "SCHEMES"
    | Facts -> "FACTS"
    | Rules -> "RULES"
    | Queries -> "QUERIES"
    | Id -> "IDENT"
    | String -> "STRING"
    | Undefined -> "UNDEFINED"
    | Eof -> "EOF"

  type t = { t_type : token_type; lexeme : string; offset : int }

  let make t_type lexeme offset = { t_type; lexeme; offset }
  let matches_type t_type token = token.t_type = t_type

  let format_t ppf token =
    Format.fprintf ppf "<offset %d: %s \"%s\">" token.offset
      (token_type_to_string token.t_type)
      token.lexeme

  let to_string token = Format.asprintf "%a" format_t token
end

type input = string

class lexer_state ?(curr_offset = 0) source =
  object (self)
    method curr_offset = curr_offset

    method peek_ahead n_chars =
      if not (n_chars >= 0) then invalid_arg "n_chars (peek_ahead)"
      else if not (self#is_in_bounds n_chars) then None
      else Some (String.get source (curr_offset + n_chars))

    method peek = self#peek_ahead 0

    method get_lexeme length =
      if (not (length >= 0)) || not (self#is_in_bounds (length - 1)) then
        invalid_arg "length (get_lexeme)"
      else String.sub source curr_offset length

    method advance n_chars =
      if not (n_chars >= 0) then invalid_arg "n_chars (advance)"
      else new lexer_state ~curr_offset:(curr_offset + n_chars) source

    method is_in_bounds n_chars = curr_offset + n_chars < String.length source
    method is_at_end = not (self#is_in_bounds 0)
  end

let consume_token t_type length lex_state =
  ( Token.make t_type (lex_state#get_lexeme length) lex_state#curr_offset,
    lex_state#advance length )

let rec find_string_end ?(length = 1) lex_state =
  match lex_state#peek_ahead length with
  | Some '\'' -> length + 1
  | Some _ -> find_string_end ~length:(length + 1) lex_state
  | None -> failwith "Unimplemented string EOF handling"

let consume_string lex_state =
  if not (lex_state#peek = Some '\'') then invalid_arg "lex_state (consume_string)"
  else let string_length = find_string_end lex_state in consume_token Token.String string_length lex_state

let lex_token lex_state =
  match lex_state#peek with
  | Some ',' -> consume_token Token.Comma 1 lex_state
  | Some '.' -> consume_token Token.Period 1 lex_state
  | Some '?' -> consume_token Token.QMark 1 lex_state
  | Some '(' -> consume_token Token.LeftParen 1 lex_state
  | Some ')' -> consume_token Token.RightParen 1 lex_state
  | Some ':' -> (
      match lex_state#peek_ahead 1 with
      | Some '-' -> consume_token Token.ColonDash 2 lex_state
      | _ -> consume_token Token.Colon 1 lex_state)
  | Some '*' -> consume_token Token.Multiply 1 lex_state
  | Some '+' -> consume_token Token.Add 1 lex_state
  | Some '\'' -> consume_string lex_state
  | _ ->
      if lex_state#is_at_end then consume_token Token.Eof 0 lex_state
      else consume_token Token.Undefined 1 lex_state

let rec lex' ?(tokens = Queue.create ()) lexer_state =
  let token, lexer_state = lex_token lexer_state in
  Queue.add token tokens;
  if Token.matches_type Token.Eof token then
    Ok (tokens |> Queue.to_seq |> List.of_seq)
  else lex' ~tokens lexer_state

let lex input = new lexer_state input |> lex'
