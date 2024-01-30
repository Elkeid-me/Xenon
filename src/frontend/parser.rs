use pest::pratt_parser::{
    Assoc::{Left, Right},
    Op, PrattParser,
};
use pest::{iterators::Pair, Parser};
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
                .op(Op::infix(Rule::multiply, Left) | Op::infix(Rule::divide, Left) | Op::infix(Rule::modulus, Left))
                .op(Op::prefix(Rule::prefix_self_decrease)
                    | Op::prefix(Rule::prefix_self_increase)
                    | Op::prefix(Rule::logical_not)
                    | Op::prefix(Rule::negative)
                    | Op::prefix(Rule::positive)
                    | Op::prefix(Rule::bit_not))
                .op(Op::postfix(Rule::postfix_self_increase) | Op::postfix(Rule::postfix_self_decrease)),
        }
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> Expr {
        self.pratt_parser
            .map_primary(|pair| match pair.as_rule() {
                Rule::expression => self.parse_expr(pair),
                Rule::integer_bin => Expr::Num(i32::from_str_radix(&pair.as_str()[2..], 2).unwrap()),
                Rule::integer_oct => Expr::Num(i32::from_str_radix(pair.as_str(), 8).unwrap()),
                Rule::integer_dec => Expr::Num(i32::from_str_radix(pair.as_str(), 10).unwrap()),
                Rule::integer_hex => Expr::Num(i32::from_str_radix(&pair.as_str()[2..], 16).unwrap()),
                Rule::identifier => Expr::Identifier(pair.as_str().to_string()),
                Rule::function_call => {
                    let mut iter = pair.into_inner();
                    let identifier = iter.next().unwrap().as_str().to_string();
                    let exprs = iter.map(|p| self.parse_expr(p)).collect();
                    Expr::FunctionCall(identifier, exprs)
                }
                Rule::array_element => {
                    let mut iter = pair.into_inner();
                    let identifier = iter.next().unwrap().as_str().to_string();
                    let exprs = iter.map(|p| self.parse_expr(p)).collect();
                    Expr::ArrayElement(identifier, exprs)
                }
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
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
                Rule::bit_left_shift_assignment => Expr::BitLeftShiftAssignment(Box::new(lhs), Box::new(rhs)),
                Rule::bit_right_shift_assignment => Expr::BitRightShiftAssignment(Box::new(lhs), Box::new(rhs)),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .map_prefix(|op, rhs| match op.as_rule() {
                Rule::prefix_self_increase => Expr::PrefixSelfIncrease(Box::new(rhs)),
                Rule::prefix_self_decrease => Expr::PrefixSelfDecrease(Box::new(rhs)),
                Rule::logical_not => Expr::LogicalNot(Box::new(rhs)),
                Rule::negative => Expr::Negative(Box::new(rhs)),
                Rule::positive => Expr::Positive(Box::new(rhs)),
                Rule::address_of => Expr::AddressOf(Box::new(rhs)),
                Rule::indirection => Expr::BitNot(Box::new(rhs)),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .map_postfix(|lhs, op| match op.as_rule() {
                Rule::postfix_self_increase => Expr::PostfixSelfIncrease(Box::new(lhs)),
                Rule::postfix_self_decrease => Expr::PostfixSelfDecrease(Box::new(lhs)),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .parse(pair.into_inner())
    }

    fn parse_init_list_item(&self, pair: Pair<Rule>) -> InitializerListItem {
        match pair.as_rule() {
            Rule::initializer_list => InitializerListItem::InitializerList(Box::new(self.parse_init_list(pair))),
            Rule::expression => InitializerListItem::Expr(self.parse_expr(pair)),
            rule => {
                dbg!(rule);
                unreachable!()
            }
        }
    }

    fn parse_init_list(&self, pair: Pair<Rule>) -> InitializerList {
        pair.into_inner().map(|pair| self.parse_init_list_item(pair)).collect()
    }

    fn parse_declarations(&self, pair: Pair<Rule>) -> Declarations {
        pair.into_inner()
            .skip(1)
            .map(|pair| match pair.as_rule() {
                Rule::const_variable_definition => {
                    let mut iter = pair.into_inner();
                    DeclarationItem::ConstVariableDefinition(
                        iter.next().unwrap().as_str().to_string(),
                        self.parse_expr(iter.next().unwrap()),
                    )
                }
                Rule::variable_definition => {
                    let mut iter = pair.into_inner();
                    DeclarationItem::VariableDefinition(
                        iter.next().unwrap().as_str().to_string(),
                        match iter.next() {
                            Some(p) => Some(self.parse_expr(p)),
                            None => None,
                        },
                    )
                }
                Rule::const_array_definition => {
                    let mut iter = pair.into_inner();
                    DeclarationItem::ConstArrayDefinition {
                        identifier: iter.next().unwrap().as_str().to_string(),
                        lengths: iter
                            .next()
                            .unwrap()
                            .into_inner()
                            .map(|expr| self.parse_expr(expr))
                            .collect(),
                        init_list: self.parse_init_list(iter.next().unwrap()),
                    }
                }
                Rule::array_definition => {
                    let mut iter = pair.into_inner();
                    DeclarationItem::ArrayDefinition {
                        identifier: iter.next().unwrap().as_str().to_string(),
                        lengths: iter
                            .next()
                            .unwrap()
                            .into_inner()
                            .map(|expr| self.parse_expr(expr))
                            .collect(),
                        init_list: match iter.next() {
                            Some(iter) => Some(self.parse_init_list(iter)),
                            None => None,
                        },
                    }
                }
                _ => {
                    dbg!(pair);
                    unreachable!()
                }
            })
            .collect()
    }

    fn parse_if_while_helper(&self, pair: Pair<Rule>) -> Block {
        match pair.as_rule() {
            Rule::block => self.parse_block(pair),
            Rule::statement => vec![BlockItem::Statement(Box::new(self.parse_statement(pair)))],
            Rule::declarations => vec![BlockItem::Declarations(Box::new(self.parse_declarations(pair)))],
            rule => {
                dbg!(rule);
                unreachable!()
            }
        }
    }
    fn parse_if(&self, pair: Pair<Rule>) -> Statement {
        let mut iter = pair.into_inner();
        Statement::If {
            condition: self.parse_expr(iter.next().unwrap()),
            then_block: self.parse_if_while_helper(iter.next().unwrap()),
            else_block: match iter.next() {
                Some(pair) => self.parse_if_while_helper(pair),
                None => Block::new(),
            },
        }
    }

    fn parse_while(&self, pair: Pair<Rule>) -> Statement {
        let mut iter = pair.into_inner();
        Statement::While {
            condition: self.parse_expr(iter.next().unwrap()),
            block: self.parse_if_while_helper(iter.next().unwrap()),
        }
    }

    fn parse_statement(&self, pair: Pair<Rule>) -> Statement {
        let iter = pair.into_inner().next().unwrap();
        match iter.as_rule() {
            Rule::expression => Statement::Expr(self.parse_expr(iter)),
            Rule::return_statement => match iter.into_inner().skip(1).next() {
                Some(exp) => Statement::Return(Some(self.parse_expr(exp))),
                None => Statement::Return(None),
            },
            Rule::if_statement => self.parse_if(iter),
            Rule::while_statement => self.parse_while(iter),
            Rule::break_statement => Statement::Break,
            Rule::continue_statement => Statement::Continue,
            rule => {
                dbg!(rule);
                unreachable!()
            }
        }
    }

    fn parse_block(&self, pair: Pair<Rule>) -> Block {
        pair.into_inner()
            .map(|pair| match pair.as_rule() {
                Rule::block => BlockItem::Block(Box::new(self.parse_block(pair))),
                Rule::statement => BlockItem::Statement(Box::new(self.parse_statement(pair))),
                Rule::declarations => BlockItem::Declarations(Box::new(self.parse_declarations(pair))),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .collect()
    }

    fn parse_signature(&self, pair: Pair<Rule>) -> (bool, String, Vec<Parameter>) {
        let mut iter = pair.into_inner();
        let return_void = match iter.next().unwrap().as_rule() {
            Rule::int => false,
            Rule::void => true,
            rule => {
                dbg!(rule);
                unreachable!()
            }
        };
        let identifier = iter.next().unwrap().as_str().to_string();
        let parameter_list = iter
            .next()
            .unwrap()
            .into_inner()
            .map(|pair| match pair.as_rule() {
                Rule::variable_parameter_definition => {
                    Parameter::Int(pair.into_inner().skip(1).next().unwrap().as_str().to_string())
                }
                Rule::array_parameter_definition => Parameter::Array(
                    pair.into_inner().skip(1).next().unwrap().as_str().to_string(),
                    Vec::new(),
                ),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .collect();
        (return_void, identifier, parameter_list)
    }

    fn parse_function_definition(&self, pair: Pair<Rule>) -> GlobalItem {
        let mut iter = pair.into_inner();
        let signature = self.parse_signature(iter.next().unwrap());
        GlobalItem::FunctionDefinition {
            return_void: signature.0,
            identifier: signature.1,
            parameter_list: signature.2,
            block: self.parse_block(iter.next().unwrap()),
        }
    }

    fn parse_global_item(&self, pair: Pair<Rule>) -> GlobalItem {
        match pair.as_rule() {
            Rule::declarations => GlobalItem::Declaration(self.parse_declarations(pair)),
            Rule::function_definition => self.parse_function_definition(pair),
            Rule::EOI => GlobalItem::EOI,
            rule => {
                dbg!(rule);
                unreachable!()
            }
        }
    }

    pub fn build_ast(&self, code: &str) -> TranslationUnit {
        let translation_unit = SysYParser::parse(Rule::translation_unit, code).unwrap();
        translation_unit
            .into_iter()
            .map(|p| Box::new(self.parse_global_item(p)))
            .filter(|p| match p.as_ref() {
                GlobalItem::Declaration(_)
                | GlobalItem::FunctionDefinition {
                    return_void: _,
                    identifier: _,
                    parameter_list: _,
                    block: _,
                } => true,
                _ => false,
            })
            .collect()
    }
}

#[test]
fn t() {
    let s = "int f(int x, int z[]){int return_ = 1, y[2] = {1, 2}; if (x == z[1]) { return_ = 2; return x == return_; } else { return 1 == 1; } }";
    let w = AstBuilder::new()
        .parse_function_definition(SysYParser::parse(Rule::function_definition, s).unwrap().next().unwrap());
    dbg!(w);

    let a = AstBuilder::new().build_ast(s);
    dbg!(a);
    assert!(false);
}
