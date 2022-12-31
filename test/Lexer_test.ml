open OUnit2
open Datalog_interpreter.Lexer

module Util = struct
  let lexer_result_to_string (lexer_result : (Token.t list, input) result) =
    match lexer_result with
    | Ok token_list ->
        "["
        ^ (token_list |> List.map Token.to_string |> String.concat ", ")
        ^ "]"
    | Error error -> "Error: " ^ error
end

let simple_token_results : (string * Token.token_type) list =
  [
    (",", Token.Comma);
    (".", Token.Period);
    ("?", Token.QMark);
    ("(", Token.LeftParen);
    (")", Token.RightParen);
    (":-", Token.ColonDash);
    (":", Token.Colon);
    ("*", Token.Multiply);
    ("+", Token.Add);
    ("Schemes", Token.Schemes);
    ("Facts", Token.Facts);
    ("Rules", Token.Rules);
    ("Queries", Token.Queries);
    ("abc", Token.Id);
    ("'abc'", Token.String);
    ("$", Token.Undefined);
  ]

let simple_token_test =
  "simple token test"
  >::: (simple_token_results
       |> List.map (fun (input, expected) -> (input, lex input, expected))
       |> List.map (fun (input, lex_result, expected) ->
              ( input,
                lex_result,
                Ok
                  [
                    Token.make expected input 0;
                    Token.make Token.Eof "" (String.length input);
                  ] ))
       |> List.map (fun (input, lex_result, expected) ->
              "`" ^ input ^ "` test" >:: fun _ ->
              assert_equal ~printer:Util.lexer_result_to_string expected
                lex_result))

(* TODO: Write tests for ignoring comments and whitespace *)
let ignore_comment_results () = assert false
let run () = run_test_tt_main simple_token_test
