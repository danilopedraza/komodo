use crate::parser::ASTNode;

#[derive(PartialEq, Eq, Debug)]
enum AnalyzerError {
  UnexpectedNodeError(ASTNode, ASTNode),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Symbol {
  val: String,
}

impl Symbol {
  fn analyze(node: ASTNode) -> Result<Self, AnalyzerError> {
    match node {
      ASTNode::Symbol(val) => Ok(Self { val }),
      node => Err(
        AnalyzerError::UnexpectedNodeError(
          ASTNode::Symbol(String::from("")),
          node
        )
      ),
    }
  }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol() {
        assert_eq!(
          Symbol::analyze(ASTNode::Symbol(String::from("x"))),
          Ok(Symbol { val: String::from("x") })
        );
    }

    #[test]
    fn expected_symbol() {
      assert_eq!(
        Symbol::analyze(ASTNode::Integer(String::from("0"))),
        Err(
          AnalyzerError::UnexpectedNodeError(
            ASTNode::Symbol(String::from("")),
            ASTNode::Integer(String::from("0"))
          )
        )
      );
    }
}
