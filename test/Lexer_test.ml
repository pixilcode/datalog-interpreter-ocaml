open OUnit2
open Datalog_interpreter.Lexer

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
                    Token.make_token expected input 0;
                    Token.make_token Token.Eof input (String.length input);
                  ] ))
       |> List.map (fun (input, lex_result, expected) ->
              "`" ^ input ^ "` test" >:: fun _ ->
              assert_equal expected lex_result))

(* TODO: Write tests for ignoring comments and whitespace *)
let ignore_comment_results () = assert false
let run () = run_test_tt_main simple_token_test
