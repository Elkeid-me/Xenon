use std::fs::read_to_string;

use pest::pratt_parser::{
    Assoc::{Left, Right},
    Op, PrattParser,
};
use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

use super::ast::*;

#[derive(Parser)]
#[grammar = "frontend/sysy.pest"]
struct SysYParser;

pub struct AstBuilder {
    pratt_parser: PrattParser<Rule>,
}

impl AstBuilder {
    pub fn new() -> AstBuilder {
        Self {
            pratt_parser: PrattParser::new()
                .op(Op::infix(Rule::assignment, Right)
                    | Op::infix(Rule::add_assignment, Right)
                    | Op::infix(Rule::subtract_assignment, Right)
                    | Op::infix(Rule::multiply_assignment, Right)
                    | Op::infix(Rule::divide_assignment, Right)
                    | Op::infix(Rule::modulus_assignment, Right)
                    | Op::infix(Rule::bit_and_assignment, Right)
                    | Op::infix(Rule::bit_or_assignment, Right)
                    | Op::infix(Rule::bit_xor_assignment, Right)
                    | Op::infix(Rule::bit_left_shift_assignment, Right)
                    | Op::infix(Rule::bit_right_shift_assignment, Right))
                .op(Op::infix(Rule::logical_or, Left))
                .op(Op::infix(Rule::logical_and, Left))
                .op(Op::infix(Rule::bit_xor, Left))
                .op(Op::infix(Rule::bit_xor, Left))
                .op(Op::infix(Rule::bit_and, Left))
                .op(Op::infix(Rule::equal, Left) | Op::infix(Rule::not_equal, Left))
                .op(Op::infix(Rule::greater, Left)
                    | Op::infix(Rule::greater_or_equal, Left)
                    | Op::infix(Rule::less, Left)
                    | Op::infix(Rule::less_or_equal, Left))
                .op(Op::infix(Rule::bit_left_shift, Left) | Op::infix(Rule::bit_right_shift, Left))
                .op(Op::infix(Rule::add, Left) | Op::infix(Rule::subtract, Left))
                .op(Op::infix(Rule::multiply, Left)
                    | Op::infix(Rule::divide, Left)
                    | Op::infix(Rule::modulus, Left))
                .op(Op::prefix(Rule::prefix_self_decrease)
                    | Op::prefix(Rule::prefix_self_increase)
                    | Op::prefix(Rule::logical_not)
                    | Op::prefix(Rule::negative)
                    | Op::prefix(Rule::positive)
                    | Op::prefix(Rule::bit_not))
                .op(Op::postfix(Rule::postfix_self_increase)
                    | Op::postfix(Rule::postfix_self_decrease)),
        }
    }

    fn parse_expr(self: &Self, pairs: Pairs<Rule>) -> Expr {
        self.pratt_parser
            .map_primary(|p| match p.as_rule() {
                Rule::expression => self.parse_expr(p.into_inner()),
                Rule::integer_bin => Expr::Num(i32::from_str_radix(&p.as_str()[2..], 2).unwrap()),
                Rule::integer_oct => Expr::Num(i32::from_str_radix(p.as_str(), 8).unwrap()),
                Rule::integer_dec => Expr::Num(i32::from_str_radix(p.as_str(), 10).unwrap()),
                Rule::integer_hex => Expr::Num(i32::from_str_radix(&p.as_str()[2..], 16).unwrap()),
                Rule::identifier => Expr::Identifier(p.as_str().into()),
                Rule::function_call => {
                    let mut iter = p.into_inner();
                    let identifier = iter.next().unwrap().as_str().into();
                    let exprs = iter.map(|p| self.parse_expr(p.into_inner())).collect();
                    Expr::FunctionCall(identifier, exprs)
                }
                Rule::array_element => {
                    let mut iter = p.into_inner();
                    let identifier = iter.next().unwrap().as_str().into();
                    let exprs = iter.map(|p| self.parse_expr(p.into_inner())).collect();
                    Expr::ArrayElement(identifier, exprs)
                }
                _ => unreachable!(),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::multiply => Expr::Multiply(Box::new(lhs), Box::new(rhs)),
                Rule::divide => Expr::Divide(Box::new(lhs), Box::new(rhs)),
                Rule::modulus => Expr::Modulus(Box::new(lhs), Box::new(rhs)),
                Rule::add => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Rule::subtract => Expr::Subtract(Box::new(lhs), Box::new(rhs)),

                Rule::logical_and => Expr::LogicalAnd(Box::new(lhs), Box::new(rhs)),
                Rule::logical_or => Expr::LogicalOr(Box::new(lhs), Box::new(rhs)),

                Rule::bit_left_shift => Expr::BitLeftShift(Box::new(lhs), Box::new(rhs)),
                Rule::bit_right_shift => Expr::BitRightShift(Box::new(lhs), Box::new(rhs)),
                Rule::bit_xor => Expr::BirXor(Box::new(lhs), Box::new(rhs)),
                Rule::bit_and => Expr::BitAnd(Box::new(lhs), Box::new(rhs)),
                Rule::bit_or => Expr::BitOr(Box::new(lhs), Box::new(rhs)),

                Rule::equal => Expr::Equal(Box::new(lhs), Box::new(rhs)),
                Rule::not_equal => Expr::NotEqual(Box::new(lhs), Box::new(rhs)),
                Rule::greater => Expr::Greater(Box::new(lhs), Box::new(rhs)),
                Rule::greater_or_equal => Expr::GreaterOrEqual(Box::new(lhs), Box::new(rhs)),
                Rule::less => Expr::Less(Box::new(lhs), Box::new(rhs)),
                Rule::less_or_equal => Expr::LessOrEqual(Box::new(lhs), Box::new(rhs)),

                Rule::assignment => Expr::Assignment(Box::new(lhs), Box::new(rhs)),
                Rule::add_assignment => Expr::AddAssignment(Box::new(lhs), Box::new(rhs)),
                Rule::subtract_assignment => Expr::SubtractAssignment(Box::new(lhs), Box::new(rhs)),
                Rule::multiply_assignment => Expr::MultiplyAssignment(Box::new(lhs), Box::new(rhs)),
                Rule::bit_and_assignment => Expr::BitAndAssignment(Box::new(lhs), Box::new(rhs)),
                Rule::bit_or_assignment => Expr::BitOrAssignment(Box::new(lhs), Box::new(rhs)),
                Rule::bit_xor_assignment => Expr::BitXorAssignment(Box::new(lhs), Box::new(rhs)),
                Rule::bit_left_shift_assignment => {
                    Expr::BitLeftShiftAssignment(Box::new(lhs), Box::new(rhs))
                }
                Rule::bit_right_shift_assignment => {
                    Expr::BitRightShiftAssignment(Box::new(lhs), Box::new(rhs))
                }
                _ => unreachable!(),
            })
            .map_prefix(|op, rhs| match op.as_rule() {
                Rule::prefix_self_increase => Expr::PrefixSelfIncrease(Box::new(rhs)),
                Rule::prefix_self_decrease => Expr::PrefixSelfDecrease(Box::new(rhs)),
                Rule::logical_not => Expr::LogicalNot(Box::new(rhs)),
                Rule::negative => Expr::Negative(Box::new(rhs)),
                Rule::positive => Expr::Positive(Box::new(rhs)),
                Rule::address_of => Expr::AddressOf(Box::new(rhs)),
                Rule::indirection => Expr::BitNot(Box::new(rhs)),
                _ => unreachable!(),
            })
            .map_postfix(|lhs, op| match op.as_rule() {
                Rule::postfix_self_increase => Expr::PostfixSelfIncrease(Box::new(lhs)),
                Rule::postfix_self_decrease => Expr::PostfixSelfDecrease(Box::new(lhs)),
                _ => unreachable!(),
            })
            .parse(pairs)
    }

    fn parse_definition(self: &Self, pair: Pair<Rule>) -> Declaration {
        match pair.as_rule() {
            Rule::const_variable_definition => {
                Declaration::ConstVariableDefinition(todo!(), todo!())
            }
            Rule::const_array_definition => Declaration::ConstArrayDefinition {
                identifier: todo!(),
                lengths: todo!(),
                init_list: todo!(),
            },
            Rule::variable_definition => Declaration::VariableDefinition(todo!(), todo!()),
            Rule::array_definition => Declaration::ArrayDefinition {
                identifier: todo!(),
                lengths: todo!(),
                init_list: todo!(),
            },
            _ => unreachable!(),
        }
    }

    fn parse_statement(self: &Self, pair: Pair<Rule>) -> Statement {
        let p = pair.into_inner().next().unwrap();
        match p.as_rule() {
            Rule::expression => Statement::Expr(self.parse_expr(p.into_inner())),
            Rule::return_statement => match p.into_inner().next() {
                None => Statement::Return(None),
                Some(exp) => Statement::Return(Some(self.parse_expr(exp.into_inner()))),
            },
            Rule::break_statement => Statement::Break,
            Rule::continue_statement => Statement::Continue,
            _ => unreachable!(),
        }
    }

    fn parse_block(self: &Self, pair: Pair<Rule>) -> Block {
        pair.into_inner()
            .into_iter()
            .map(|pair| match pair.as_rule() {
                Rule::block => BlockItem::Block(Box::new(self.parse_block(pair))),
                Rule::statement => BlockItem::Statement(Box::new(self.parse_statement(pair))),
                Rule::const_variable_definition => todo!(),
                _ => unreachable!(),
            })
            .collect()
    }

    fn parse_function_definition(self: &Self, pair: Pair<Rule>) -> GlobalItem {
        todo!()
    }

    fn parse_global_item(self: &Self, pair: Pair<Rule>) -> GlobalItem {
        match pair.as_rule() {
            Rule::const_declaration => todo!(),
            Rule::function_declaration => todo!(),
            Rule::function_definition => todo!(),
            Rule::declaration => todo!(),
            _ => unreachable!(),
        }
    }

    pub fn build_ast(self: &Self, code: &str) -> TranslationUnit {
        let translation_unit = SysYParser::parse(Rule::translation_unit, code).unwrap();
        translation_unit
            .into_iter()
            .map(|p| Box::new(self.parse_global_item(p)))
            .collect()
    }
}

#[test]
fn t() {
    let s = "{return x + 1; return; {  z = z[1] + z[2]; } }".to_string();
    let w =
        AstBuilder::new().parse_block(SysYParser::parse(Rule::block, &s).unwrap().next().unwrap());
    dbg!(w);
    assert!(false);
}
