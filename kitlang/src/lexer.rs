use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("function")]
    Function,

    #[token("main")]
    Main,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token(";")]
    Semicolon,

    #[token("printf")]
    Printf,

    #[regex(r#""([^"\\]|\\.)*""#)]
    StringLiteral,

    #[regex(r"%[a-zA-Z]")]
    FormatSpecifier,

    #[token(",")]
    Comma,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
