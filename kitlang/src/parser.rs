use logos::Lexer;

use crate::lexer::Token;

pub fn compile(lexer: &mut Lexer<Token>) {
    while let Some(token) = next_token(lexer) {
        match token {
            Token::Function => {
                println!("Found function definition");
                expect(lexer, Token::Main, "Expected `main` after `function`");
                expect(lexer, Token::LeftParen, "Expected `(` after `main`");
                expect(lexer, Token::RightParen, "Expected `)` after `(`");
                expect(
                    lexer,
                    Token::LeftBrace,
                    "Expected `{` to begin function body",
                );

                parse_function_body(lexer);

                expect(
                    lexer,
                    Token::RightBrace,
                    "Expected `}` to close function body",
                );
            }
            other => {
                panic!("Unexpected token at top level: {:?}", other);
            }
        }
    }
}

fn parse_function_body(lexer: &mut Lexer<Token>) {
    match next_token(lexer) {
        Some(Token::Printf) => {
            println!("Found printf");
            expect(lexer, Token::LeftParen, "Expected `(` after `printf`");

            let first = next_token(lexer).expect("Expected argument inside printf");
            match first {
                Token::FormatSpecifier | Token::StringLiteral => {
                    println!("  Format/String: {:?}", first);
                }
                _ => panic!("Unexpected first argument in printf: {:?}", first),
            }

            expect(lexer, Token::Comma, "Expected `,` between printf args");

            let second = next_token(lexer).expect("Expected second argument in printf");
            match second {
                Token::StringLiteral => {
                    println!("  Argument: {:?}", second);
                }
                _ => panic!("Unexpected second argument in printf: {:?}", second),
            }

            expect(lexer, Token::RightParen, "Expected `)` after printf args");
            expect(lexer, Token::Semicolon, "Expected `;` after printf");
        }
        Some(other) => panic!("Unexpected token in function body: {:?}", other),
        None => panic!("Unexpected end of input in function body"),
    }
}

fn expect(lexer: &mut Lexer<Token>, expected: Token, msg: &str) {
    match next_token(lexer) {
        Some(t) if t == expected => {}
        Some(t) => panic!("{} (found {:?})", msg, t),
        None => panic!("{} (found end of input)", msg),
    }
}

fn next_token(lexer: &mut Lexer<Token>) -> Option<Token> {
    lexer.next().map(|result| result.expect("Lexing error"))
}
