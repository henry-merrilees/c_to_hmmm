mod lexer;

fn main() {
    loop {
        // read line from stdin
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        // lex line
        input = input.strip_suffix('\n').unwrap().to_string();
        dbg!(lexer::Token::lex(&input));
    }
}
