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

  type t = { tType : token_type; lexeme : string; offset : int }

  let make_token tType lexeme offset = { tType; lexeme; offset }
  let matches tokenA tokenB = tokenA.tType = tokenB.tType
end

type input = string

let lex _input = Error ""
