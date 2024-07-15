use crate::error::Position;

#[allow(unused)]
pub struct ASTNode {
    _type: ASTNodeType,
    position: Position,
}

impl ASTNode {
    pub fn new(_type: ASTNodeType, position: Position) -> Self {
        Self { _type, position }
    }
}

#[allow(unused)]
pub enum ASTNodeType {
    Boolean(bool),
    Call {
        called: Box<ASTNode>,
        args: Vec<ASTNode>,
    },
    Char(char),
    ComprehensionList {
        val: Box<ASTNode>,
        prop: Box<ASTNode>,
    },
    ComprehensionSet {
        val: Box<ASTNode>,
        prop: Box<ASTNode>,
    },
    Decimal {
        int: String,
        dec: String,
    },
    ExtensionList {
        list: Vec<ASTNode>,
    },
    ExtensionSet {
        list: Vec<ASTNode>,
    },
    For {
        val: String,
        iter: Box<ASTNode>,
        proc: Vec<ASTNode>,
    },
    Function {
        params: Vec<String>,
        proc: Vec<ASTNode>,
    },
    Fraction {
        numer: Box<ASTNode>,
        denom: Box<ASTNode>,
    },
}
