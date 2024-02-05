use pest::pratt_parser::{
    Assoc::{Left, Right},
    Op, PrattParser,
};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use super::ast::{ArithmeticOp::*, AssignOp::*, Expr::*, InfixOp::*, UnaryOp::*, *};

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
                    | Op::prefix(Rule::bit_not)
                    | Op::prefix(Rule::indirection))
                .op(Op::postfix(Rule::postfix_self_increase) | Op::postfix(Rule::postfix_self_decrease)),
        }
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> Expr {
        self.pratt_parser
            .map_primary(|pair| match pair.as_rule() {
                Rule::expression => self.parse_expr(pair),
                Rule::integer_bin => Num(i32::from_str_radix(&pair.as_str()[2..], 2).unwrap()),
                Rule::integer_oct => Num(i32::from_str_radix(pair.as_str(), 8).unwrap()),
                Rule::integer_dec => Num(i32::from_str_radix(pair.as_str(), 10).unwrap()),
                Rule::integer_hex => Num(i32::from_str_radix(&pair.as_str()[2..], 16).unwrap()),
                Rule::identifier => Identifier(pair.as_str().to_string()),
                Rule::function_call => {
                    let mut iter = pair.into_inner();
                    let identifier = iter.next().unwrap().as_str().to_string();
                    let exprs = iter.map(|p| self.parse_expr(p)).collect();
                    FunctionCall(identifier, exprs)
                }
                Rule::array_element => {
                    let mut iter = pair.into_inner();
                    let identifier = iter.next().unwrap().as_str().to_string();
                    let expr = self.parse_expr(iter.next().unwrap());
                    ArrayElement(identifier, Box::new(expr))
                }
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::multiply => InfixExpr(Box::new(lhs), Arith(Multiply), Box::new(rhs)),
                Rule::divide => InfixExpr(Box::new(lhs), Arith(Divide), Box::new(rhs)),
                Rule::modulus => InfixExpr(Box::new(lhs), Arith(Modulus), Box::new(rhs)),
                Rule::add => InfixExpr(Box::new(lhs), Arith(Add), Box::new(rhs)),
                Rule::subtract => InfixExpr(Box::new(lhs), Arith(Subtract), Box::new(rhs)),

                Rule::logical_and => InfixExpr(Box::new(lhs), Arith(LogicalAnd), Box::new(rhs)),
                Rule::logical_or => InfixExpr(Box::new(lhs), Arith(LogicalOr), Box::new(rhs)),

                Rule::bit_left_shift => InfixExpr(Box::new(lhs), Arith(BitLeftShift), Box::new(rhs)),
                Rule::bit_right_shift => InfixExpr(Box::new(lhs), Arith(BitRightShift), Box::new(rhs)),
                Rule::bit_xor => InfixExpr(Box::new(lhs), Arith(BirXor), Box::new(rhs)),
                Rule::bit_and => InfixExpr(Box::new(lhs), Arith(BitAnd), Box::new(rhs)),
                Rule::bit_or => InfixExpr(Box::new(lhs), Arith(BitOr), Box::new(rhs)),

                Rule::equal => InfixExpr(Box::new(lhs), Arith(Equal), Box::new(rhs)),
                Rule::not_equal => InfixExpr(Box::new(lhs), Arith(NotEqual), Box::new(rhs)),
                Rule::greater => InfixExpr(Box::new(lhs), Arith(Greater), Box::new(rhs)),
                Rule::greater_or_equal => InfixExpr(Box::new(lhs), Arith(GreaterOrEqual), Box::new(rhs)),
                Rule::less => InfixExpr(Box::new(lhs), Arith(Less), Box::new(rhs)),
                Rule::less_or_equal => InfixExpr(Box::new(lhs), Arith(LessOrEqual), Box::new(rhs)),

                Rule::assignment => InfixExpr(Box::new(lhs), Assign(Assignment), Box::new(rhs)),
                Rule::add_assignment => InfixExpr(Box::new(lhs), Assign(AddAssign), Box::new(rhs)),
                Rule::subtract_assignment => InfixExpr(Box::new(lhs), Assign(SubtractAssign), Box::new(rhs)),
                Rule::multiply_assignment => InfixExpr(Box::new(lhs), Assign(MultiplyAssign), Box::new(rhs)),
                Rule::bit_and_assignment => InfixExpr(Box::new(lhs), Assign(BitAndAssign), Box::new(rhs)),
                Rule::bit_or_assignment => InfixExpr(Box::new(lhs), Assign(BitOrAssign), Box::new(rhs)),
                Rule::bit_xor_assignment => InfixExpr(Box::new(lhs), Assign(BitXorAssign), Box::new(rhs)),
                Rule::bit_left_shift_assignment => InfixExpr(Box::new(lhs), Assign(BitLeftShiftAssign), Box::new(rhs)),
                Rule::bit_right_shift_assignment => InfixExpr(Box::new(lhs), Assign(BitRightShiftAssign), Box::new(rhs)),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .map_prefix(|op, rhs| match op.as_rule() {
                Rule::prefix_self_increase => UnaryExpr(PrefixSelfIncrease, Box::new(rhs)),
                Rule::prefix_self_decrease => UnaryExpr(PrefixSelfDecrease, Box::new(rhs)),
                Rule::logical_not => UnaryExpr(LogicalNot, Box::new(rhs)),
                Rule::negative => UnaryExpr(Negative, Box::new(rhs)),
                Rule::positive => UnaryExpr(Positive, Box::new(rhs)),
                Rule::address_of => UnaryExpr(AddressOf, Box::new(rhs)),
                Rule::bit_not => UnaryExpr(BitNot, Box::new(rhs)),
                Rule::indirection => UnaryExpr(Indirection, Box::new(rhs)),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .map_postfix(|lhs, op| match op.as_rule() {
                Rule::postfix_self_increase => UnaryExpr(PostfixSelfIncrease, Box::new(lhs)),
                Rule::postfix_self_decrease => UnaryExpr(PostfixSelfDecrease, Box::new(lhs)),
                rule => {
                    dbg!(rule);
                    unreachable!()
                }
            })
            .parse(pair.into_inner())
    }

    // fn parse_init_list_item(&self, pair: Pair<Rule>) -> InitializerListItem {
    //     match pair.as_rule() {
    //         Rule::initializer_list => InitializerListItem::InitializerList(Box::new(self.parse_init_list(pair))),
    //         Rule::expression => InitializerListItem::Expr(self.parse_expr(pair)),
    //         rule => {
    //             dbg!(rule);
    //             unreachable!()
    //         }
    //     }
    // }

    fn parse_init_list(&self, pair: Pair<Rule>) -> InitializerList {
        pair.into_inner().map(|pair| self.parse_expr(pair)).collect()
    }

    fn parse_definition(&self, pair: Pair<Rule>) -> Definition {
        match pair.as_rule() {
            Rule::const_variable_definition => {
                let mut iter = pair.into_inner();
                Definition::ConstVariableDefinition(
                    iter.next().unwrap().as_str().to_string(),
                    self.parse_expr(iter.next().unwrap()),
                )
            }
            Rule::variable_definition => {
                let mut iter = pair.into_inner();
                Definition::VariableDefinition(
                    iter.next().unwrap().as_str().to_string(),
                    match iter.next() {
                        Some(p) => Some(self.parse_expr(p)),
                        None => None,
                    },
                )
            }
            Rule::const_array_definition => {
                let mut iter = pair.into_inner();
                Definition::ConstArrayDefinition {
                    identifier: iter.next().unwrap().as_str().to_string(),
                    // lengths: iter.next().unwrap().into_inner().map(|expr| self.parse_expr(expr)).collect(),
                    length: self.parse_expr(iter.next().unwrap()),
                    init_list: self.parse_init_list(iter.next().unwrap()),
                }
            }
            Rule::array_definition => {
                let mut iter = pair.into_inner();
                Definition::ArrayDefinition {
                    identifier: iter.next().unwrap().as_str().to_string(),
                    // lengths: iter.next().unwrap().into_inner().map(|expr| self.parse_expr(expr)).collect(),
                    length: self.parse_expr(iter.next().unwrap()),
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
        }
    }

    fn parse_if_while_helper(&self, pair: Pair<Rule>) -> Block {
        match pair.as_rule() {
            Rule::block => self.parse_block(pair),
            Rule::statement => vec![BlockItem::Statement(Box::new(self.parse_statement(pair)))],
            Rule::definitions_in_if_or_while_non_block => pair
                .into_inner()
                .skip(1)
                .map(|pair| BlockItem::Definition(Box::new(self.parse_definition(pair))))
                .collect(),
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
            .filter(|pair| !matches!(pair.as_rule(), Rule::int | Rule::const_definition_type))
            .map(|pair| match pair.as_rule() {
                Rule::block => BlockItem::Block(Box::new(self.parse_block(pair))),
                Rule::statement => BlockItem::Statement(Box::new(self.parse_statement(pair))),
                Rule::variable_definition
                | Rule::array_definition
                | Rule::const_variable_definition
                | Rule::const_array_definition => BlockItem::Definition(Box::new(self.parse_definition(pair))),
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
                Rule::array_parameter_definition => {
                    Parameter::Pointer(pair.into_inner().skip(1).next().unwrap().as_str().to_string())
                }
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
            Rule::variable_definition | Rule::array_definition | Rule::const_variable_definition | Rule::const_array_definition => {
                GlobalItem::Definition(self.parse_definition(pair))
            }
            Rule::function_definition => self.parse_function_definition(pair),
            rule => {
                dbg!(rule);
                unreachable!()
            }
        }
    }

    pub fn build_ast(&self, code: &str) -> TranslationUnit {
        let translation_unit = SysYParser::parse(Rule::translation_unit, code).unwrap();
        translation_unit
            .filter(|pair| !matches!(pair.as_rule(), Rule::EOI | Rule::int | Rule::const_definition_type))
            .map(|p| Box::new(self.parse_global_item(p)))
            .collect()
    }
}
