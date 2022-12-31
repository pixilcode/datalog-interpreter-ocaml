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

  type t = { t_type : token_type; lexeme : string; offset : int }

  val make : token_type -> string -> int -> t
  val matches_type : token_type -> t -> bool

  (* Formatting *)
  val format_t : Format.formatter -> t -> unit
  val to_string : t -> string
end

type input = string

val lex : input -> (Token.t list, string) result
