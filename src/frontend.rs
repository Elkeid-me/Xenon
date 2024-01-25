use pest::pratt_parser::{Assoc, Op};
use pest::{error::Error, iterators::Pair, pratt_parser::PrattParser, Parser};
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
                .op(Op::infix(Rule::logic_or, Assoc::Left))
                .op(Op::infix(Rule::logic_and, Assoc::Left))
                .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::subtract, Assoc::Left))
                .op(Op::infix(Rule::multiply, Assoc::Left) | Op::infix(Rule::divide, Assoc::Left)),
        }
    }
    fn parse_expr(self: &Self) -> Result<AstNode, Error<Rule>> {
        todo!()
    }

    fn generate_ast(self: &Self, code: &String) -> Result<AstNode, Error<Rule>> {
        let translation_unit = SYSYParser::parse(Rule::translation_unit, code)?
            .next()
            .unwrap();
        todo!()
    }

    pub fn generate_ir(self: &Self, code: &String) -> koopa::ir::Program {
        self.parse_expr();
        self.parse_expr();
        todo!()
    }
}
