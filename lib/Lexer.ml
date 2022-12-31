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
      if not (self#is_in_bounds n_chars) then None
      else Some (String.get source (curr_offset + n_chars))

    method peek = self#peek_ahead 0

    method get_lexeme length =
      if (not (length >= 0)) || not (self#is_in_bounds (length - 1)) then
        invalid_arg "length (get_lexeme)"
      else String.sub source curr_offset length

    method advance n_chars =
      new lexer_state ~curr_offset:(curr_offset + n_chars) source

    method is_in_bounds n_chars = curr_offset + n_chars < String.length source
  end

let consume_token t_type length lex_state =
  ( Token.make t_type (lex_state#get_lexeme length) lex_state#curr_offset,
    lex_state#advance length )

let rec find_string_length ?(curr_char = 1) lex_state =
  match lex_state#peek_ahead curr_char with
  | Some '\'' -> curr_char + 1
  | Some _ -> find_string_length ~curr_char:(curr_char + 1) lex_state
  | None -> failwith "Unimplemented string EOF handling"

let consume_string lex_state =
  let string_length = find_string_length lex_state in
  consume_token Token.String string_length lex_state

let is_alpha input_char =
  let input_char_code = Char.code input_char in
  (input_char_code >= (Char.code 'A') && input_char_code <= (Char.code 'Z')) ||
  (input_char_code >= (Char.code 'a') && input_char_code <= (Char.code 'z'))

let is_numeric input_char =
  let input_char_code = Char.code input_char in
  input_char_code >= (Char.code '0') && input_char_code <= (Char.code '9')

let is_alphanumeric input_char = is_alpha input_char || is_numeric input_char

let rec find_ident_length ?(curr_char = 1) lex_state =
  match lex_state#peek_ahead curr_char with
  | Some c when is_alphanumeric c -> find_ident_length ~curr_char:(curr_char + 1) lex_state
  | _ -> curr_char

let consume_identifier lex_state =
  let ident_length = find_ident_length lex_state in
  let t_type = (match lex_state#get_lexeme ident_length with
    | "Schemes" -> Token.Schemes
    | "Facts" -> Token.Facts
    | "Rules" -> Token.Rules
    | "Queries" -> Token.Queries
    | _ -> Token.Id) in
  consume_token t_type ident_length lex_state

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
  | Some c when is_alpha c -> consume_identifier lex_state
  | Some _ -> consume_token Token.Undefined 1 lex_state
  | None -> consume_token Token.Eof 0 lex_state

let rec lex' ?(tokens = Queue.create ()) lexer_state =
  let token, lexer_state = lex_token lexer_state in
  Queue.add token tokens;
  if Token.matches_type Token.Eof token then
    Ok (tokens |> Queue.to_seq |> List.of_seq)
  else lex' ~tokens lexer_state

let lex input = new lexer_state input |> lex'
