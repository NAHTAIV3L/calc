#[derive(PartialEq, Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    #[allow(dead_code)]
    fn name(self) -> String {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
        }.into()
    }
}

#[derive(PartialEq, Clone)]
enum Token {
    Number(String),
    Operator(Op),
    Oparen,
    Cparen,
    End,
}

impl Token {
    #[allow(dead_code)]
    fn name(self) -> String {
        match self {
            Token::Number(x)   => x,
            Token::Operator(x) => x.name(),
            Token::Oparen         => "(".to_string(),
            Token::Cparen         => ")".to_string(),
            Token::End         => "<end>".to_string(),
        }.into()
    }
}

struct Lexer {
    index: usize,
    tokens: Vec<Token>
}

impl Lexer {
    fn peek(&self) -> Token {
        self.tokens.get(self.index).cloned().unwrap_or(Token::End)
    }

    fn next(&mut self) -> Token {
        let tok = self.peek();
        self.index += 1;
        tok
    }
}

#[derive(Clone)]
enum Node {
    Immediate(f32),
    BinOp(Op, Box<Node>, Box<Node>),
    MonOp(Op, Box<Node>),
    Paren(Box<Node>)
}

impl Node {
    #[allow(dead_code)]
    fn name(self) -> String {
        match self {
            Node::Immediate(x) => format!("imm: {}", x),
            Node::BinOp(a, b, c) => format!("({} {} {})", a.name(), b.name(), c.name()),
            Node::MonOp(a, b) => format!("{} {}", a.name(), b.name()),
            Node::Paren(a) => format!("(Paren: {})", a.name()),
        }.into()
    }
}

fn op_prec(op: Op) -> i32 {
    match op {
        Op::Add | Op::Sub => 5,
        Op::Mul | Op::Div => 6,
    }
}

fn parse_monop(lexer: &mut Lexer) -> Node {
    match lexer.next() {
        Token::Operator(op) => {
            if op == Op::Add || op == Op::Sub {
                lexer.next();
                let arg = parse_monop(lexer);
                return Node::MonOp(op, Box::new(arg));
            }
            else {
                return match lexer.peek() {
                    Token::Number(x) => Node::Immediate(x.parse::<f32>().unwrap()),
                    _ => Node::Immediate(0.0)
                }
            }
        }
        Token::Oparen => {
            let arg = parse_binop(lexer, 0);
            if Token::Cparen  == lexer.next() {
                return Node::Paren(Box::new(arg))
            }
            else {
                println!("Expected )");
                std::process::exit(1);
            }
        }
        Token::Number(x) => {
            return Node::Immediate(x.parse::<f32>().unwrap())
        }
        _ => {
            Node::Immediate(0.0)
        }
    }
}

fn parse_binop(lexer: &mut Lexer, prec: i32) -> Node {
    let mut lhs = parse_monop(lexer);

    loop {
        match lexer.peek() {
            Token::Operator(op) if prec <= op_prec(op) => {
                lexer.next();
                let rhs = parse_binop(lexer, op_prec(op) + 1);
                lhs = Node::BinOp(op, Box::new(lhs), Box::new(rhs))
            }
            _ => break lhs
        }
    }
}

fn parse(lexer : &mut Lexer) -> Node {
    return parse_binop(lexer, 0);
}

fn evaluate_binop(op: Op, lhs :&f32, rhs: &f32) -> f32 {
    match (op, lhs.clone(), rhs.clone()) {
        (Op::Add, x, y) => x + y,
        (Op::Sub, x, y) => x - y,
        (Op::Mul, x, y) => x * y,
        (Op::Div, x, y) => x / y,
    }
}

fn evaluate_monop(op: Op, arg: &f32) -> f32 {
    match (op, arg) {
        (Op::Add, x) => *x,
        (Op::Sub, x) => -*x,
        _ => {
            println!("Invalid monop");
            std::process::exit(1);
        }
    }
}

fn evaluate(node: &Node) -> f32 {
    match node {
        Node::Immediate(x) => x.clone(),
        Node::BinOp(op, lhs, rhs) => {
            let x = evaluate(lhs);
            let y = evaluate(rhs);
            evaluate_binop(*op, &x, &y)
        }
        Node::MonOp(op, arg) => {
            let x = evaluate(arg);
            evaluate_monop(*op, &x)
        }
        Node::Paren(arg) => {
            evaluate(arg)
        }
    }
}

fn tokenize(equation : String) -> Lexer {
    let mut v : Vec<Token> = vec![];
    let mut tempstr = "".to_string();
    for c in equation.chars() {
        if c.is_numeric() || c == '.' {
            tempstr.push(c);
        }
        else {
            if tempstr.len() != 0 {
                v.push(Token::Number(tempstr.clone()));
                tempstr.clear();
            }
            if c.is_whitespace() {
                continue;
            }
            else if c == '+' {
                v.push(Token::Operator(Op::Add));
            }
            else if c == '-' {
                v.push(Token::Operator(Op::Sub));
            }
            else if c == '*' {
                v.push(Token::Operator(Op::Mul));
            }
            else if c == '/' {
                v.push(Token::Operator(Op::Div));
            }
            else if c == '(' {
                match v.last() {
                    Some(x) => {
                        match x {
                            Token::Operator(_) => { v.push(Token::Oparen) }
                            _ => {
                                v.push(Token::Operator(Op::Mul));
                                v.push(Token::Oparen);
                            }
                        }
                    }
                    None => {
                        v.push(Token::Oparen);
                    }
                }
            }
            else if c == ')' {
                v.push(Token::Cparen);
            }
            else {
                println!("Invalid Operator!");
                std::process::exit(1);
            }
        }
    }
    if tempstr.len() != 0 {
        v.push(Token::Number(tempstr.clone()));
        tempstr.clear();
    }
    Lexer {
        index: 0,
        tokens: v
    }
}

fn main() {
    let mut arg = std::env::args();
    if arg.len() < 2 {
        println!("usage: {} [math expression]", arg.nth(0).ok_or("No program").unwrap());
        std::process::exit(1);
    }
    let out = arg.nth(1).unwrap();
    let mut lexer = tokenize(out.clone());

    #[cfg(debug_assertions)]
    {
        for t in lexer.tokens.clone() {
            print!("{}", t.name());
        }
        println!("");
    }

    let node = parse(&mut lexer);

    #[cfg(debug_assertions)]
    println!("{}", node.clone().name());

    println!("{}", evaluate(&node));
}
