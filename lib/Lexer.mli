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

  type t = { tType : token_type; lexeme : string; offset : int }

  val make_token : token_type -> string -> int -> t
  val matches : t -> t -> bool
end

type input = string

val lex : input -> (Token.t list, string) result
