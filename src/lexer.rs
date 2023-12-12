use regex::Regex;

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Debug, Clone)]
pub enum Token {
    Type(String),
    Idenitfier(String),
    Constant(String),
    StringLiteral(String),
    COMMENT,
    AUTO,
    BREAK,
    CASE,
    CHAR,
    CONST,
    CONTINUE,
    DEFAULT,
    DO,
    DOUBLE,
    ELSE,
    ENUM,
    EXTERN,
    FLOAT,
    FOR,
    GOTO,
    IF,
    INT,
    LONG,
    REGISTER,
    RETURN,
    SHORT,
    SIGNED,
    SIZEOF,
    STATIC,
    STRUCT,
    SWITCH,
    TYPEDEF,
    UNION,
    UNSIGNED,
    VOID,
    VOLATILE,
    WHILE,
    ELLIPSIS,
    RIGHT_ASSIGN,
    LEFT_ASSIGN,
    ADD_ASSIGN,
    SUB_ASSIGN,
    MUL_ASSIGN,
    DIV_ASSIGN,
    MOD_ASSIGN,
    AND_ASSIGN,
    XOR_ASSIGN,
    OR_ASSIGN,
    RIGHT_OP,
    LEFT_OP,
    INC_OP,
    DEC_OP,
    PTR_OP,
    AND_OP,
    OR_OP,
    LE_OP,
    GE_OP,
    EQ_OP,
    NE_OP,
    SEMICOLON,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    COLON,
    EQUALS,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    PERIOD,
    AMPERSAND,
    BANG,
    TILDE,
    MINUS,
    PLUS,
    ASTERISK,
    FORWARD_SLASH,
    PERCENT,
    LT,
    GT,
    CARROT,
    VERTICAL_BAR,
    QUESTION_MARK,
}

impl Token {
    /*
    0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
    0{D}+{IS}?		{ count(); return(CONSTANT); }
    {D}+{IS}?		{ count(); return(CONSTANT); }
    L?'(\\.|[^\\'])+'	{ count(); return(CONSTANT); }

    {D}+{E}{FS}?		{ count(); return(CONSTANT); }
    {D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
    {D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }
    */
    fn is_constant(candidate: &str) -> bool {
        let digit = r"[0-9]";
        let letter = r"[a-zA-Z_]";
        let hex = r"[a-fA-F0-9]";
        let exponent = format!(r"[Ee][+-]?{digit}+");
        let float_specifier = r"(f|F|l|L)";
        let integer_specifier = r"(u|U|l|L)*";

        let hex_regex = Regex::new(format!(r"0[xX]{hex}+{integer_specifier}?").as_str()).unwrap();
        let zero_decimal_regex =
            Regex::new(format!(r"0{digit}+{integer_specifier}?").as_str()).unwrap();
        let decimal_regex = Regex::new(format!(r"{digit}+{integer_specifier}?").as_str()).unwrap();
        // not sure what's going on here
        let char_regex = Regex::new(format!(r"{letter}?'(\\.|[^\\'])+'").as_str()).unwrap();
        let float_regex_1 =
            Regex::new(format!(r"{digit}+{exponent}{float_specifier}?").as_str()).unwrap();
        let float_regex_2 =
            Regex::new(format!(r"{digit}*\.{digit}+({exponent})?{float_specifier}?").as_str())
                .unwrap();
        let float_regex_3 =
            Regex::new(format!(r"{digit}+\.{digit}*({exponent})?{float_specifier}?").as_str())
                .unwrap();

        hex_regex.is_match(candidate)
            || zero_decimal_regex.is_match(candidate)
            || decimal_regex.is_match(candidate)
            || char_regex.is_match(candidate)
            || float_regex_1.is_match(candidate)
            || float_regex_2.is_match(candidate)
            || float_regex_3.is_match(candidate)
    }
    pub fn lex(keyword: &str) -> Option<Token> {
        use Token::*;
        match keyword {
            "/*" => return Some(COMMENT),
            "auto" => return Some(AUTO),
            "break" => return Some(BREAK),
            "case" => return Some(CASE),
            "char" => return Some(CHAR),
            "const" => return Some(CONST),
            "continue" => return Some(CONTINUE),
            "default" => return Some(DEFAULT),
            "do" => return Some(DO),
            "double" => return Some(DOUBLE),
            "else" => return Some(ELSE),
            "enum" => return Some(ENUM),
            "extern" => return Some(EXTERN),
            "float" => return Some(FLOAT),
            "for" => return Some(FOR),
            "goto" => return Some(GOTO),
            "if" => return Some(IF),
            "int" => return Some(INT),
            "long" => return Some(LONG),
            "register" => return Some(REGISTER),
            "return" => return Some(RETURN),
            "short" => return Some(SHORT),
            "signed" => return Some(SIGNED),
            "sizeof" => return Some(SIZEOF),
            "static" => return Some(STATIC),
            "struct" => return Some(STRUCT),
            "switch" => return Some(SWITCH),
            "typedef" => return Some(TYPEDEF),
            "union" => return Some(UNION),
            "unsigned" => return Some(UNSIGNED),
            "void" => return Some(VOID),
            "volatile" => return Some(VOLATILE),
            "while" => return Some(WHILE),
            _ => {}
        };

        let mut char_vec = keyword.chars();

        if char_vec.next().is_some_and(|c| c.is_ascii_alphabetic()) {
            let mut is_valid_ident: bool = true;
            for c in char_vec {
                if !c.is_ascii_alphanumeric() {
                    is_valid_ident = false;
                    break;
                }
            }

            if is_valid_ident {
                if todo!("check if keyword is a type") {
                    return Some(Type(keyword.to_string()));
                } else {
                    return Some(Idenitfier(keyword.to_string()));
                }
            }
        }

        if Self::is_constant(keyword) {
            return Some(Constant(keyword.to_string()));
        }

        let string_literal_regex = Regex::new(r#"L?"(\\.|[^\\"])*""#).unwrap(); // TODO check
        if string_literal_regex.is_match(keyword) {
            return Some(StringLiteral(keyword.to_string()));
        }

        match keyword {
            "..." => return Some(ELLIPSIS),
            ">>=" => return Some(RIGHT_ASSIGN),
            "<<=" => return Some(LEFT_ASSIGN),
            "+=" => return Some(ADD_ASSIGN),
            "-=" => return Some(SUB_ASSIGN),
            "*=" => return Some(MUL_ASSIGN),
            "/=" => return Some(DIV_ASSIGN),
            "%=" => return Some(MOD_ASSIGN),
            "&=" => return Some(AND_ASSIGN),
            "^=" => return Some(XOR_ASSIGN),
            "|=" => return Some(OR_ASSIGN),
            ">>" => return Some(RIGHT_OP),
            "<<" => return Some(LEFT_OP),
            "++" => return Some(INC_OP),
            "--" => return Some(DEC_OP),
            "->" => return Some(PTR_OP),
            "&&" => return Some(AND_OP),
            "||" => return Some(OR_OP),
            "<=" => return Some(LE_OP),
            ">=" => return Some(GE_OP),
            "==" => return Some(EQ_OP),
            "!=" => return Some(NE_OP),
            ";" => return Some(SEMICOLON),
            "{" | "<%" => return Some(LEFT_BRACE),
            "}" | "%>" => return Some(RIGHT_BRACE),
            "," => return Some(COMMA),
            ":" => return Some(COLON),
            "=" => return Some(EQUALS),
            "(" => return Some(LEFT_PAREN),
            ")" => return Some(RIGHT_PAREN),
            "[" | "<:" => return Some(LEFT_BRACKET),
            "]" | ":>" => return Some(RIGHT_BRACKET),
            "." => return Some(PERIOD),
            "&" => return Some(AMPERSAND),
            "!" => return Some(BANG),
            "~" => return Some(TILDE),
            "-" => return Some(MINUS),
            "+" => return Some(PLUS),
            "*" => return Some(ASTERISK),
            "/" => return Some(FORWARD_SLASH),
            "%" => return Some(PERCENT),
            "<" => return Some(LT),
            ">" => return Some(GT),
            "^" => return Some(CARROT),
            "|" => return Some(VERTICAL_BAR),
            "?" => return Some(QUESTION_MARK),
            _ => {}
        }

        //ignore whitespace
        None
    }
}
