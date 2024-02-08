use super::{
    ast::*,
    expr::types::Type::{self, *},
};
use std::{collections::HashMap, mem::take, vec};
pub enum SymbolTableItem<'a> {
    ConstVariable(i32),
    Variable,
    ConstArray(&'a Vec<usize>, &'a ConstInitializerList),
    Array(&'a Vec<usize>),
    Function(Type<'a>, Vec<Type<'a>>),
    Pointer(&'a Vec<usize>),
}

/// 每个人承担自己的风险！
#[macro_export]
macro_rules! risk {
    ($expression:expr, $pattern:pat => $extracted_expression:expr) => {
        match $expression {
            $pattern => $extracted_expression,
            _ => unreachable!(),
        }
    };
}

use SymbolTableItem::{Array, ConstArray, ConstVariable, Function, Variable};

pub type SymbolTable<'a> = Vec<HashMap<&'a str, SymbolTableItem<'a>>>;

pub trait Scope<'a> {
    fn search(&self, identifier: &str) -> Option<&SymbolTableItem>;

    fn insert_definition(&mut self, identifier: &'a str, symbol: SymbolTableItem<'a>) -> Result<(), String>;

    fn enter_scope(&mut self);
    fn exit_scope(&mut self);
}

impl<'a> Scope<'a> for SymbolTable<'a> {
    fn search(&self, identifier: &str) -> Option<&SymbolTableItem> {
        for map in self.iter().rev() {
            if let Some(info) = map.get(identifier) {
                return Some(info);
            }
        }
        return None;
    }

    fn insert_definition(&mut self, identifier: &'a str, symbol: SymbolTableItem<'a>) -> Result<(), String> {
        match self.last_mut().unwrap().insert(identifier, symbol) {
            Some(_) => Err(format!("标识符 {} 在当前作用域中已存在", identifier)),
            None => Ok(()),
        }
    }

    fn enter_scope(&mut self) {
        self.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.pop();
    }
}

pub struct Checker<'a> {
    pub table: SymbolTable<'a>,
}

impl<'a> Checker<'a> {
    pub fn new() -> Self {
        Self {
            table: vec![HashMap::from([
                ("getint", Function(Int, Vec::new())),
                ("getch", Function(Int, Vec::new())),
                ("getarray", Function(Int, vec![Pointer(&[])])),
                ("putint", Function(Void, vec![Int])),
                ("putch", Function(Void, vec![Int])),
                ("putarray", Function(Int, vec![Int, Pointer(&[])])),
                ("starttime", Function(Void, Vec::new())),
                ("stoptime", Function(Void, Vec::new())),
            ])],
        }
    }

    fn process_init_list(&self, init_list: &mut InitializerList, lengths: &[usize]) -> Result<InitializerList, String> {
        todo!()
    }

    fn process_const_init_list(&self, init_list: &mut InitializerList, lengths: &[usize]) -> Result<ConstInitializerList, String> {
        let prod = lengths.iter().fold(1usize, |l, r| l * *r);
        for ele in init_list.iter_mut() {
            match ele {
                InitializerListItem::InitializerList(_) => todo!(),
                InitializerListItem::Expr(_) => todo!(),
            }
        }
        todo!()
    }

    fn process_definition(&mut self, definition: &'a mut Definition) -> Result<(), String> {
        match definition {
            Definition::ConstVariableDefinitionTmp(identifier, init) => {
                *definition = Definition::ConstVariableDefinition(take(identifier), init.const_eval(&self.table)?);
                let (identifier, init) = risk!(definition, Definition::ConstVariableDefinition(id, i) => (id, *i));
                self.table.insert_definition(identifier, ConstVariable(init))
            }
            Definition::ConstArrayDefinitionTmp {
                identifier,
                lengths,
                init_list,
            } => {
                for expr in lengths.iter_mut() {
                    expr.const_eval(&self.table)?;
                }
                let lengths: Vec<usize> = lengths.iter_mut().map(|p| risk!(p, Expr::Num(i) => *i as usize)).collect();
                let init_list = self.process_const_init_list(init_list, &lengths)?;
                *definition = Definition::ConstArrayDefinition {
                    identifier: take(identifier),
                    lengths,
                    init_list,
                };
                let (identifier, lengths, init_list) = risk!(definition, Definition::ConstArrayDefinition { identifier, lengths, init_list } => (identifier, lengths, init_list));
                self.table.insert_definition(identifier, ConstArray(lengths, init_list))
            }
            Definition::VariableDefinition(identifier, init) => {
                if let Some(expr) = init {
                    if !matches!(expr.expr_type(&self.table)?, Int) {
                        return Err(format!("{:?} 不是整型表达式", expr));
                    }
                }
                self.table.insert_definition(identifier, Variable)
            }
            Definition::ArrayDefinitionTmp {
                identifier,
                lengths,
                init_list,
            } => {
                for expr in lengths.iter_mut() {
                    expr.const_eval(&self.table)?;
                }
                let lengths: Vec<usize> = lengths.iter_mut().map(|p| risk!(p, Expr::Num(i) => *i as usize)).collect();
                let init_list = match init_list {
                    Some(init_list) => Some(self.process_init_list(init_list, &lengths)?),
                    None => None,
                };
                *definition = Definition::ArrayDefinition {
                    identifier: take(identifier),
                    lengths,
                    init_list,
                };
                todo!()
            }
            _ => unreachable!(),
        }
    }

    fn process_block(&mut self, block: &'a mut Block, return_void: bool, in_while: bool) -> Result<(), String> {
        self.table.enter_scope();
        for block_item in block.iter_mut() {
            match block_item {
                BlockItem::Definition(definition) => self.process_definition(definition)?,
                BlockItem::Block(block) => self.process_block(block, return_void, in_while)?,
                BlockItem::Statement(statement) => match statement.as_mut() {
                    Statement::Expr(expr) => expr.check_expr(&self.table)?,
                    Statement::If {
                        condition,
                        then_block,
                        else_block,
                    } => match condition.expr_type(&self.table)? {
                        Void => return Err(format!("{:?} 不能作为 if 的条件", condition)),
                        _ => {
                            self.process_block(then_block, return_void, in_while)?;
                            self.process_block(else_block, return_void, in_while)?;
                        }
                    },
                    Statement::While { condition, block } => match condition.expr_type(&self.table)? {
                        Void => return Err(format!("{:?} 不能作为 if 的条件", condition)),
                        _ => self.process_block(block, return_void, in_while)?,
                    },
                    Statement::Return(expr) => match (expr, return_void) {
                        (None, true) => (),
                        (None, false) => return Err("int 函数中的 return 语句未返回表达式".to_string()),
                        (Some(expr), true) => return Err(format!("在 void 函数中返回了表达式 {:?}", expr)),
                        (Some(expr), false) => {
                            if !matches!(expr.expr_type(&self.table)?, Int) {
                                return Err(format!("return 语句返回的 {:?} 类型与函数定义不匹配", expr));
                            }
                        }
                    },
                    Statement::Break | Statement::Continue => {
                        if !in_while {
                            return Err("在 while 语句外使用了 break 或 continue".to_string());
                        }
                    }
                },
            }
        }
        self.table.exit_scope();
        Ok(())
    }

    pub fn check(&mut self, ast: &'a mut TranslationUnit) -> Result<(), String> {
        for i in ast.iter_mut() {
            match i.as_mut() {
                GlobalItem::Definition(definition) => self.process_definition(definition)?,
                GlobalItem::FunctionDefinition {
                    return_void,
                    identifier,
                    parameter_list,
                    block,
                } => {
                    for p in parameter_list.iter_mut() {
                        if let Parameter::PointerTmp(identifier, exprs) = p {
                            for expr in exprs.iter_mut() {
                                expr.const_eval(&self.table)?;
                            }
                            *p = Parameter::Pointer(
                                take(identifier),
                                exprs.iter().map(|p| risk!(p, Expr::Num(i) => *i as usize)).collect(),
                            )
                        }
                    }
                    let parameter_type = parameter_list
                        .iter()
                        .map(|p| match p {
                            Parameter::Int(_) => Int,
                            Parameter::Pointer(_, lengths) => Type::Pointer(lengths),
                            Parameter::PointerTmp(_, _) => unreachable!(),
                        })
                        .collect();
                    let return_type = if *return_void { Void } else { Int };
                    self.table
                        .insert_definition(identifier, Function(return_type, parameter_type))?;
                    self.table.enter_scope();
                    for p in parameter_list.iter() {
                        match p {
                            Parameter::Int(identifier) => self.table.insert_definition(identifier, Variable)?,
                            Parameter::Pointer(identifier, lengths) => {
                                self.table.insert_definition(identifier, SymbolTableItem::Pointer(lengths))?
                            }
                            Parameter::PointerTmp(_, _) => unreachable!(),
                        }
                    }
                    self.process_block(block, *return_void, false)?;
                    self.table.exit_scope();
                }
            }
        }
        Ok(())
    }
}
