open OUnit2
open Datalog_interpreter.Lexer
open Datalog_interpreter.Lexer.Token

let simple_token_results : (string * token_type) list = [
    (",", Comma);
    (".", Period);
    ("?", QMark);
    ("(", LeftParen);
    (")", RightParen);
    (":-", ColonDash);
    (":", Colon);
    ("*", Multiply);
    ("+", Add);
    ("Schemes", Schemes);
    ("Facts", Facts);
    ("Rules", Rules);
    ("Queries", Queries);
    ("abc", Id);
    ("'abc'", String);
    ("$", Undefined);
  ]

let make_token lexeme tType offset = {
        tType;
        lexeme;
        offset;
    }

let simple_token_test = "simple token test" >::: (
        simple_token_results
        |> List.map (fun (input, expected) -> (input, lex input, expected))
        |> List.map (fun (input, lex_result, expected) -> (
            input,
            lex_result,
            Ok [
                (make_token input expected 0);
                (make_token input Eof (String.length input))
            ]
        ))
        |> List.map (fun (input, lex_result, expected) ->
            "`" ^ input ^ "` test" >:: (fun _ -> assert_equal expected lex_result)
        )
    )

(* TODO: Write tests for ignoring comments and whitespace *)
let ignore_comment_results () = assert false

let run () = run_test_tt_main simple_token_test
