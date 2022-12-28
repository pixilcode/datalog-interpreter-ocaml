module Token : sig
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

  type token = {
    tType: token_type;
    lexeme: string;
    offset: int;
  }

  val matches : token -> token -> bool
end

type input = string

val lex : input -> (Token.token list, string) result 
