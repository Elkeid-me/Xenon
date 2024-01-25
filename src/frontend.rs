use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};
use pest_derive::Parser;

mod ast;
use ast::AstNode;
#[derive(Parser)]
#[grammar = "frontend/sysy.pest"]
struct SYSYParser;

pub struct Frontend {
    pratt_parser: PrattParser<Rule>,
}

impl Frontend {
    pub fn new() -> Frontend {
        Frontend {
            pratt_parser: PrattParser::new()
                .op(Op::infix(Rule::assign, Assoc::Left))
                .op(Op::infix(Rule::logical_or, Assoc::Left))
                .op(Op::infix(Rule::logical_and, Assoc::Left))
                .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::subtract, Assoc::Left))
                .op(Op::infix(Rule::multiply, Assoc::Left)
                    | Op::infix(Rule::divide, Assoc::Left)
                    | Op::infix(Rule::modulus, Assoc::Left))
                .op(Op::prefix(Rule::prefix_self_decrease)
                    | Op::prefix(Rule::prefix_self_increase)
                    | Op::prefix(Rule::logical_not)
                    | Op::prefix(Rule::negative)
                    | Op::prefix(Rule::positive))
                .op(Op::postfix(Rule::postfix_self_increase)
                    | Op::postfix(Rule::postfix_self_decrease)),
        }
    }
    fn parse_expr(self: &Self, pairs: Pairs<Rule>) -> Result<AstNode, Error<Rule>> {
        let w = self
            .pratt_parser
            .map_primary(|primary| AstNode::ast_node)
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::postfix_self_increase => AstNode::ast_node,
                Rule::postfix_self_decrease => AstNode::ast_node,
                rule => unreachable!(),
            })
            .map_prefix(|op, rhs| match op.as_rule() {
                Rule::postfix_self_increase => AstNode::ast_node,
                Rule::postfix_self_decrease => AstNode::ast_node,
                rule => unreachable!(),
            })
            .map_postfix(|lhs, op| match op.as_rule() {
                Rule::postfix_self_increase => AstNode::ast_node,
                Rule::postfix_self_decrease => AstNode::ast_node,
                rule => unreachable!(),
            })
            .parse(pairs);
        todo!()
    }

    fn generate_ast(self: &Self, code: &String) -> Result<AstNode, Error<Rule>> {
        let translation_unit = SYSYParser::parse(Rule::translation_unit, code)?
            .next()
            .unwrap();
        fn parse_ast_node(pair: Pair<Rule>) -> AstNode {
            todo!()
        }
        todo!()
    }

    pub fn generate_ir(self: &Self, code: &String) -> koopa::ir::Program {
        todo!()
    }
}
