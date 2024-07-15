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
    Call(Box<ASTNode>, Vec<ASTNode>),
}
