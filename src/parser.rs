use std::{iter::Peekable, vec};

use crate::lexer::Token;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    LogicOr,
    LogicAnd,
    Comparison,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Shift,
    Addition,
    Multiplication,
    Exponentiation,
    Correspondence,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InfixOperator {
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Correspondence,
    Division,
    Equality,
    Exponentiation,
    Greater,
    GreaterEqual,
    LeftShift,
    Less,
    LessEqual,
    LogicAnd,
    LogicOr,
    Mod,
    NotEquality,
    Product,
    RightShift,
    Substraction,
    Sum,
}

impl InfixOperator {
    fn from(tok: Token) -> Option<Self> {
        match tok {
            Token::BitwiseAnd => Some(Self::BitwiseAnd),
            Token::BitwiseOr => Some(Self::BitwiseOr),
            Token::BitwiseXor => Some(Self::BitwiseXor),
            Token::Greater => Some(Self::Greater),
            Token::GreaterEqual => Some(Self::GreaterEqual),
            Token::LeftShift => Some(Self::LeftShift),
            Token::RightShift => Some(Self::RightShift),
            Token::Less => Some(Self::Less),
            Token::LessEqual => Some(Self::LessEqual),
            Token::LogicAnd => Some(Self::LogicAnd),
            Token::LogicOr => Some(Self::LogicOr),
            Token::Mod => Some(Self::Mod),
            Token::Over => Some(Self::Division),
            Token::Plus => Some(Self::Sum),
            Token::Minus => Some(Self::Substraction),
            Token::Times => Some(Self::Product),
            Token::ToThe => Some(Self::Exponentiation),
            Token::Arrow => Some(Self::Correspondence),
            Token::Equals => Some(Self::Equality),
            Token::NotEqual => Some(Self::NotEquality),
            _ => None,
        }
    }

    fn precedence(&self) -> Precedence {
        match self {
            Self::BitwiseAnd => Precedence::BitwiseAnd,
            Self::BitwiseOr => Precedence::BitwiseOr,
            Self::BitwiseXor => Precedence::BitwiseXor,
            Self::Correspondence => Precedence::Correspondence,
            Self::Division => Precedence::Multiplication,
            Self::Equality => Precedence::Comparison,
            Self::Exponentiation => Precedence::Exponentiation,
            Self::Greater => Precedence::Comparison,
            Self::GreaterEqual => Precedence::Comparison,
            Self::LeftShift => Precedence::Shift,
            Self::Less => Precedence::Comparison,
            Self::LessEqual => Precedence::Comparison,
            Self::LogicAnd => Precedence::LogicAnd,
            Self::LogicOr => Precedence::LogicOr,
            Self::Mod => Precedence::Multiplication,
            Self::NotEquality => Precedence::Comparison,
            Self::Product => Precedence::Multiplication,
            Self::RightShift => Precedence::Shift,
            Self::Substraction => Precedence::Addition,
            Self::Sum => Precedence::Addition,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ASTNode {
    Boolean(bool),
    ComprehensionSet(Box<ASTNode>, Box<ASTNode>),
    ExtensionSet(Vec<ASTNode>),
    Infix(InfixOperator, Box<ASTNode>, Box<ASTNode>),
    Integer(String),
    Let(Box<ASTNode>, Vec<ASTNode>, Box<ASTNode>),
    Signature(Box<ASTNode>, Option<Box<ASTNode>>),
    Symbol(String),
    Tuple(Vec<ASTNode>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    UnexpectedTokenError(Vec<Token>, Token),
    EOFError,
    EOFErrorExpecting(Vec<Token>),
}


pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.peek() {
            None => None,
            _ => Some(self.expression(Precedence::Lowest)),
        }
    }
}

type NodeResult = Result<ASTNode, ParserError>;

impl <T: Iterator<Item = Token>> Parser<T> {
    fn let_(&mut self) -> NodeResult {
        let sg = self.signature()?;

        match self.tokens.next() {
            Some(Token::Assign) => self.expression(Precedence::Lowest).map(
                |res| ASTNode::Let(Box::new(sg), vec![], Box::new(res))
            ),
            _ => Ok(sg),
        }
    }

    fn signature(&mut self) -> NodeResult {
        match (self.tokens.next(), self.tokens.peek()) {
            (Some(Token::Ident(name)), Some(Token::Colon)) => {
                self.tokens.next();
                self.type_().map(
                    |tp| ASTNode::Signature(
                        Box::new(ASTNode::Symbol(name)),
                        Some(Box::new(tp))
                    )
                )
            },
            (Some(Token::Ident(name)), Some(Token::Lparen)) => {
                self.tokens.next();
                self.function_with_arguments(name)
            },
            (Some(Token::Ident(name)), _) => Ok(
                ASTNode::Signature(
                    Box::new(
                        ASTNode::Symbol(name)
                    ),
                    None
                )
            ),
            (Some(tok), _) => Err(
                ParserError::UnexpectedTokenError(
                    vec![Token::Ident(String::from(""))],
                    tok
                )
            ),
            (None, _) => Err(
                ParserError::EOFErrorExpecting(
                    vec![Token::Ident(String::from(""))]
                )
            ),
        }
    }

    fn function_with_arguments(&mut self, name: String) -> NodeResult {
        let args_res = self.list(Token::Rparen, None);

        match (args_res, self.tokens.next()) {
            (Ok(args), Some(Token::Assign)) => match self.expression(Precedence::Lowest) {
                Ok(expr) => Ok(
                    ASTNode::Let(
                        Box::new(ASTNode::Symbol(name)),
                        args,
                        Box::new(expr)
                    )
                ),
                err => err,
            },
            (Err(err), _) => Err(err),
            (_, Some(tok)) => Err(
                ParserError::UnexpectedTokenError(
                    vec![Token::Assign],
                    tok
                )
            ),
            (Ok(_), None) => Err(
                ParserError::EOFErrorExpecting(
                    vec![Token::Assign]
                )
            ),
        }
    }

    fn list(&mut self, terminator: Token, first: Option<ASTNode>) -> Result<Vec<ASTNode>, ParserError> {
        let mut res = match first {
            None => vec![],
            Some(node) => vec![node],
        };

        match self.tokens.peek() {
            Some(tok) if *tok == terminator => {
                self.tokens.next();   
                Ok(res)
            }
            None => Err(ParserError::EOFError),
            _ => loop {
                let expr = self.expression(Precedence::Lowest)?;
                res.push(expr);

                match self.tokens.next() {
                    Some(Token::Comma) => continue,
                    Some(tok) if tok == terminator => break Ok(res),
                    Some(tok) => return Err(
                        ParserError::UnexpectedTokenError(
                            vec![Token::Comma, terminator],
                            tok
                        )
                    ),
                    None => return Err(
                        ParserError::EOFErrorExpecting(
                            vec![Token::Comma, terminator]
                        )
                    ),
                }
            },
        }
    }

    fn expression(&mut self, precedence: Precedence) -> NodeResult {
        let mut expr = match self.tokens.next() {
            None => Err(ParserError::EOFError),
            Some(tok) => match tok {
                Token::Let => self.let_(),
                Token::True => Ok(ASTNode::Boolean(true)),
                Token::False => Ok(ASTNode::Boolean(false)),
                Token::Lparen => self.parenthesis(),
                Token::Lbrace => self.set(),
                Token::Integer(int) => Ok(ASTNode::Integer(int)),
                Token::Ident(literal) => Ok(ASTNode::Symbol(literal)),
                tok => Err(
                    ParserError::UnexpectedTokenError(
                        vec![
                            Token::Lparen,
                            Token::Lbrace,
                            Token::Integer(String::from("")),
                            Token::Ident(String::from(""))
                        ],
                        tok
                    )
                ),
            },
        }?;

        while let Some(op) = self.current_infix() {
            if precedence < op.precedence() {
                self.tokens.next();
                expr = self.infix(expr, op)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn current_infix(&mut self) -> Option<InfixOperator> {
        self.tokens.peek().and_then(|tok| InfixOperator::from(tok.clone()))
    }

    fn parenthesis(&mut self) -> NodeResult {
        if self.tokens.next_if_eq(&Token::Rparen).is_some() {
            return Ok(ASTNode::Tuple(vec![]));
        }

        let res = self.expression(Precedence::Lowest)?;

        match self.tokens.next() {
            Some(Token::Rparen) => Ok(res),
            Some(Token::Comma) => self.list(
                Token::Rparen,
                Some(res)).map(ASTNode::Tuple),
            Some(tok) => Err(
                ParserError::UnexpectedTokenError(
                    vec![Token::Rparen],
                    tok
                )
            ),
            None => Err(
                ParserError::EOFErrorExpecting(
                    vec![Token::Rparen]
                )
            ),
        }
    }

    fn infix(&mut self, lhs: ASTNode, op: InfixOperator) -> NodeResult {
        self.expression(op.precedence()).map(
            |rhs| ASTNode::Infix(op, Box::new(lhs), Box::new(rhs))
        )
    }

    fn type_(&mut self) -> NodeResult {
        self.expression(Precedence::Lowest)
    }

    fn set(&mut self) -> NodeResult {
        if matches!(self.tokens.peek(), Some(&Token::Rbrace)) {
            return Ok(ASTNode::ExtensionSet(vec![]));
        }

        let first_res = self.expression(Precedence::Lowest);

        match (first_res, self.tokens.next()) {
            (Ok(first), Some(Token::Comma) | Some(Token::Rbrace)) => self.list(
                Token::Rbrace,
                Some(first)
            ).map(ASTNode::ExtensionSet),
            (Ok(first), Some(Token::Colon)) => self.expression(Precedence::Lowest)
            .map(
                |second| ASTNode::ComprehensionSet(
                    Box::new(first),
                    Box::new(second)
                )
            ),
            (Ok(_), Some(tok)) => Err(
                ParserError::UnexpectedTokenError(
                    vec![
                        Token::Comma,
                        Token::Rbrace, 
                        Token::Colon
                    ],
                    tok
                )
            ),
            (Ok(_), None) => Err(
                ParserError::EOFErrorExpecting(
                    vec![
                        Token::Comma,
                        Token::Rbrace,
                        Token::Colon
                    ]
                )
            ),
            (err, _) => err,
        }
    }
}

pub fn parser_from<T: Iterator<Item = Token>>(tokens: T) -> Parser<T> {
    Parser { tokens: tokens.peekable() }
}

#[cfg(test)]
mod tests {
    use std::{iter, vec};
    use crate::lexer::{build_lexer, Token};
    use super::*;

    macro_rules! token_iter {
        ($v:expr) => {
            $v.iter().map(|tok| tok.clone())
        };
    }

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(iter::empty::<Token>()).next(), None);
    }

    #[test]
    fn integer() {
        let tokens = vec![Token::Integer(String::from("0"))];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Integer(String::from("0"))))
        );
    }

    #[test]
    fn integer_in_parenthesis() {
        let tokens = vec![Token::Lparen, Token::Integer(String::from("365")), Token::Rparen];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Integer(String::from("365"))))
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = vec![Token::Lparen, Token::Integer(String::from("65"))];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(ParserError::EOFErrorExpecting(vec![Token::Rparen])))
        );
    }

    #[test]
    fn simple_sum() {
        let tokens = vec![Token::Integer(String::from("1")), Token::Plus, Token::Integer(String::from("1"))];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Integer(String::from("1"))),
                    Box::new(ASTNode::Integer(String::from("1")))
                )
            ))
        );
    }

    #[test]
    fn incomplete_sum() {
        let tokens = vec![Token::Integer(String::from("1")), Token::Plus];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(ParserError::EOFError))
        );
    }

    #[test]
    fn product_and_power() {
        let tokens = vec![
            Token::Integer(String::from("1")), Token::Times,
            Token::Integer(String::from("2")), Token::ToThe, Token::Integer(String::from("2"))
        ];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Infix(
                    InfixOperator::Product,
                    Box::new(ASTNode::Integer(String::from("1"))),
                    Box::new(
                        ASTNode::Infix(
                            InfixOperator::Exponentiation,
                            Box::new(
                                ASTNode::Integer(String::from("2"))
                            ),
                            Box::new(
                                ASTNode::Integer(String::from("2"))
                            ),
                        )
                    )
                )
            ))
        );
    }

    #[test]
    fn division_and_sum() {
        let tokens = vec![Token::Integer(String::from("1")), Token::Over,
                                      Token::Integer(String::from("1")), Token::Plus, Token::Integer(String::from("1"))];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Infix(
                        InfixOperator::Division,
                        Box::new(ASTNode::Integer(String::from("1"))),
                        Box::new(ASTNode::Integer(String::from("1")))
                    )),
                    Box::new(ASTNode::Integer(String::from("1")))
                )
            )),
        );
    }

    #[test]
    fn let_statement() {
        let tokens = vec![Token::Let, Token::Ident(String::from('x')), Token::Assign, Token::Integer(String::from("1"))];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Let(
                    Box::new(
                        ASTNode::Signature(
                            Box::new(
                                ASTNode::Symbol(
                                    String::from('x')
                                )
                            ),
                            None
                        )
                    ),
                    vec![],
                    Box::new(ASTNode::Integer(String::from("1")))
                )
            )),
        );
    }

    #[test]
    fn comparison_precedence() {
        let tokens = vec![
            Token::Integer(String::from("1")), Token::Plus,
            Token::Integer(String::from("5")),
            Token::NotEqual,
            Token::Integer(String::from("6")), Token::Mod,
            Token::Integer(String::from("2"))
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::NotEquality,
                Box::new(
                    ASTNode::Infix(
                        InfixOperator::Sum,
                        Box::new(
                            ASTNode::Integer(String::from("1"))
                        ),
                        Box::new(
                            ASTNode::Integer(String::from("5"))
                        ),
                    )
                ),
                Box::new(
                    ASTNode::Infix(
                        InfixOperator::Mod,
                        Box::new(
                            ASTNode::Integer(String::from("6"))
                        ),
                        Box::new(
                            ASTNode::Integer(String::from("2"))
                        ),
                    )
                )
            )))
        );
    }

    #[test]
    fn let_function_statement() {
        let tokens = vec![
            Token::Let, Token::Ident(String::from('f')), Token::Lparen ,Token::Ident(String::from('x')), Token::Comma, Token::Ident(String::from('y')), Token::Rparen,
            Token::Assign,
            Token::Ident(String::from('x')), Token::Plus, Token::Ident(String::from('y'))
        ];
        
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Let(
                    Box::new(ASTNode::Symbol(String::from('f'))),
                    vec![ASTNode::Symbol(String::from('x')), ASTNode::Symbol(String::from('y'))],
                    Box::new(
                        ASTNode::Infix(
                            InfixOperator::Sum,
                            Box::new(ASTNode::Symbol(String::from('x'))),
                            Box::new(ASTNode::Symbol(String::from('y')))
                        )
                    )
                )
            )),
        );
    }

    #[test]
    fn let_function_signature() {
        let tokens = vec![
            Token::Let, Token::Ident(String::from('f')), Token::Colon,
            Token::Ident(String::from('a')), Token::Arrow,
            Token::Ident(String::from('a'))
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Signature(
                    Box::new(ASTNode::Symbol(String::from('f'))),
                    Some(
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::Correspondence,
                                Box::new(ASTNode::Symbol(String::from('a'))),
                                Box::new(ASTNode::Symbol(String::from('a')))
                            )
                        )
                    )
                )
            )),
        );
    }

    #[test]
    fn empty_set() {
        let tokens = vec![Token::Lbrace, Token::Rbrace];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::ExtensionSet(vec![])))
        );
    }

    #[test]
    fn set() {
        let tokens = vec![
            Token::Lbrace, Token::Lparen, Token::True,
            Token::Rparen, Token::Comma, Token::False,
            Token::Rbrace
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::ExtensionSet(
                    vec![ASTNode::Boolean(true), ASTNode::Boolean(false)]
                )
            ))
        );
    }

    #[test]
    fn empty_tuple() {
        let tokens = vec![Token::Lparen, Token::Rparen];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Tuple(vec![])))
        );
    }

    #[test]
    fn tuple() {
        let tokens = vec![
            Token::Lparen,
            Token::Ident(String::from("Real")),
            Token::Comma,
            Token::Ident(String::from("Real")),
            Token::Rparen
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(
                Ok(
                    ASTNode::Tuple(
                        vec![
                            ASTNode::Symbol(String::from("Real")),
                            ASTNode::Symbol(String::from("Real"))
                        ]
                    )
                )
            )
        );
    }

    #[test]
    fn set_comprehension() {
        let tokens = vec![
            Token::Lbrace, Token::Ident(String::from("a")),
            Token::Colon, Token::Ident(String::from("a")),
            Token::Equals, Token::Integer(String::from("1"))
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::ComprehensionSet(
                    Box::new(ASTNode::Symbol(String::from("a"))),
                    Box::new(ASTNode::Infix(
                        InfixOperator::Equality,
                        Box::new(ASTNode::Symbol(String::from("a"))),
                        Box::new(ASTNode::Integer(String::from("1")))
                    ))
                )
            ))
        );
    }

    #[test]
    fn typed_let() {
        let tokens = vec![
            Token::Let, Token::Ident(String::from("x")),
            Token::Colon, Token::Ident(String::from("Real")),
            Token::Assign, Token::Integer(String::from("1")), Token::Plus,
            Token::Integer(String::from("0")), Token::Mod, Token::Integer(String::from("2")),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(
                Ok(
                    ASTNode::Let(
                        Box::new(ASTNode::Signature(
                            Box::new(ASTNode::Symbol(String::from("x"))),
                            Some(Box::new(ASTNode::Symbol(String::from("Real"))))
                        )),
                        vec![],
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::Sum,
                                Box::new(
                                    ASTNode::Integer(String::from("1"))
                                ),
                                Box::new(
                                    ASTNode::Infix(
                                        InfixOperator::Mod,
                                        Box::new(
                                            ASTNode::Integer(String::from("0"))
                                        ),
                                        Box::new(
                                            ASTNode::Integer(String::from("2"))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn shift_operator() {
        let tokens = vec![
            Token::Ident(String::from('x')), Token::Minus, Token::Integer(String::from('1')),
            Token::LeftShift,
            Token::Integer(String::from('1')) 
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(
                Ok(
                    ASTNode::Infix(
                        InfixOperator::LeftShift,
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::Substraction,
                                Box::new(
                                    ASTNode::Symbol(String::from('x'))
                                ),
                                Box::new(
                                    ASTNode::Integer(String::from('1'))
                                )
                            )
                        ),
                        Box::new(
                            ASTNode::Integer(String::from('1'))
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn shift_and_comparison() {
        let lexer = build_lexer("1 << 1 > 1");
        
        assert_eq!(
            parser_from(lexer).next(),
            Some(
                Ok(
                    ASTNode::Infix(
                        InfixOperator::Greater,
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::LeftShift,
                                Box::new(
                                    ASTNode::Integer(String::from('1'))
                                ),
                                Box::new(
                                    ASTNode::Integer(String::from('1'))
                                )
                            )
                        ),
                        Box::new(
                            ASTNode::Integer(String::from('1'))
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn bitwise() {
        let lexer = build_lexer("a & b | c");

        assert_eq!(
            parser_from(lexer).next(),
            Some(
                Ok(
                    ASTNode::Infix(
                        InfixOperator::BitwiseOr,
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::BitwiseAnd,
                                Box::new(
                                    ASTNode::Symbol(String::from('a'))
                                ),
                                Box::new(
                                    ASTNode::Symbol(String::from('b'))
                                )
                            )
                        ),
                        Box::new(
                            ASTNode::Symbol(String::from('c'))
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn logic_infix_operators() {
        let lexer = build_lexer("a && b || c");

        assert_eq!(
            parser_from(lexer).next(),
            Some(
                Ok(
                    ASTNode::Infix(
                        InfixOperator::LogicOr,
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::LogicAnd,
                                Box::new(
                                    ASTNode::Symbol(String::from('a'))
                                ),
                                Box::new(
                                    ASTNode::Symbol(String::from('b'))
                                )
                            )
                        ),
                        Box::new(
                            ASTNode::Symbol(String::from('c'))
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn complex_precedence() {
        let lexer = build_lexer("a + b || a & b << c");

        assert_eq!(
            parser_from(lexer).next(),
            Some(
                Ok(
                    ASTNode::Infix(
                        InfixOperator::LogicOr,
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::Sum,
                                Box::new(
                                    ASTNode::Symbol(String::from('a'))
                                ),
                                Box::new(
                                    ASTNode::Symbol(String::from('b'))
                                )
                            )
                        ),
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::BitwiseAnd,
                                Box::new(
                                    ASTNode::Symbol(String::from('a'))
                                ),
                                Box::new(
                                    ASTNode::Infix(
                                        InfixOperator::LeftShift,
                                        Box::new(
                                            ASTNode::Symbol(String::from('b'))
                                        ),
                                        Box::new(
                                            ASTNode::Symbol(String::from('c'))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn bitwise_xor() {
        let lexer = build_lexer("a ^ b & c | d");

        assert_eq!(
            parser_from(lexer).next(),
            Some(
                Ok(
                    ASTNode::Infix(
                        InfixOperator::BitwiseOr,
                        Box::new(
                            ASTNode::Infix(
                                InfixOperator::BitwiseXor,
                                Box::new(
                                    ASTNode::Symbol(String::from('a'))
                                ),
                                Box::new(
                                    ASTNode::Infix(
                                        InfixOperator::BitwiseAnd,
                                        Box::new(
                                            ASTNode::Symbol(String::from('b'))
                                        ),
                                        Box::new(
                                            ASTNode::Symbol(String::from('c'))
                                        )
                                    )
                                )
                            )
                        ),
                        Box::new(
                            ASTNode::Symbol(String::from('d'))
                        )
                    )
                )
            )
        );
    }
}
