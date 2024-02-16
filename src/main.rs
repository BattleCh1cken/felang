use chumsky::{error::Cheap, prelude::*};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Bool(bool),
    Num(String),
    Str(String),
    Op(String),
    Ctrl(char),
    Type(String),
    Ident(String),
    If,
    Else,
    Return,
    Assert,
    Macro,
    Define,
    Null,
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+-*/!=")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,").map(|c| Token::Ctrl(c));

    // A parser for operators
    let macro_ident = just("#")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .to(Token::Macro);

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "int" => Token::Type("int".to_string()),
        "str" => Token::Type("int".to_string()),
        "if" => Token::If,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "return" => Token::Return,
        _ => Token::Ident(ident),
    });

    let token = num
        .or(str_)
        .or(op)
        .or(ctrl)
        .or(ident)
        .or(macro_ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

pub type Spanned<T> = (T, Span);

#[derive(Debug)]
enum Type {
    Int,
    Str,
    Bool,
    Void,
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Null,
    Num(f64),
    Func(String),
    Bool(bool),
}

#[derive(Clone, Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
}

#[derive(Debug)]
enum Expr {
    Error,
    Value(Value),
    List(Vec<Spanned<Self>>),
    Local(String),
    Let(String, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
}

#[derive(Debug)]
struct Func {
    return_type: Type,
    args: Vec<String>,
    body: Spanned<Expr>,
}

fn macro_parser() {}

fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let bob = just(Token::Null).to(Expr::Value(Value::Null));
    })
    .ignore_then(end())
}

fn main() {
    let sample_input = r#"
    #-BOB

    #macro foo(x, y) {
      return x + y
    }

    // this is a comment
    int bob = 1.0

    int bob(int x, int y) {

    }
    "#;

    match lexer().parse(sample_input) {
        Ok(result) => println!("{:?}", result),
        Err(err) => panic!("error: {:?}", err),
    }
}
