use super::{
    ast::*,
    expr::{
        const_eval::{check_expr, const_eval},
        types::Type,
    },
};
use std::{collections::HashMap, vec};
pub enum SymbolTableItem<'a> {
    ConstVariable(i32),
    Variable(Option<&'a Expr>),
    ConstArray(i32),
    Array(Option<i32>),
    Function(Type, Vec<Type>),
}

pub type SymbolTable<'a> = Vec<HashMap<&'a str, SymbolTableItem<'a>>>;

pub trait Scope<'a> {
    fn new() -> Self;
    fn search(&self, identifier: &str) -> Option<&SymbolTableItem>;

    fn insert_definition(&mut self, identifier: &'a str, symbol: SymbolTableItem<'a>) -> Result<(), ()>;

    fn enter_scope(&mut self);
    fn exit_scope(&mut self);
}

impl<'a> Scope<'a> for SymbolTable<'a> {
    fn new() -> Self {
        vec![HashMap::new()]
    }

    fn search(&self, identifier: &str) -> Option<&SymbolTableItem> {
        self.last().unwrap().get(identifier)
    }

    fn insert_definition(&mut self, identifier: &'a str, symbol: SymbolTableItem<'a>) -> Result<(), ()> {
        match self.last_mut().unwrap().insert(identifier, symbol) {
            Some(_) => Err(()),
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
            table: vec![HashMap::new()],
        }
    }

    fn process_definition(&mut self, definition: &'a mut Definition) -> Result<(), ()> {
        match definition {
            Definition::ConstVariableDefinition(identifier, init) => match const_eval(init, &self.table) {
                Ok((_, _, Some(i))) => self.table.insert_definition(identifier, SymbolTableItem::ConstVariable(i)),
                _ => Err(()),
            },
            Definition::ConstArrayDefinition {
                identifier,
                lengths,
                init_list,
            } => todo!(),
            Definition::VariableDefinition(identifier, init) => match init {
                Some(expr) => {
                    const_eval(expr, &self.table)?;
                    self.table
                        .insert_definition(identifier, SymbolTableItem::Variable(Some(expr)))
                }
                None => self.table.insert_definition(identifier, SymbolTableItem::Variable(None)),
            },
            Definition::ArrayDefinition {
                identifier,
                lengths,
                init_list,
            } => todo!(),
        }
    }

    fn process_block(&mut self, block: &'a mut Block, return_void: bool, in_while: bool) -> Result<(), ()> {
        self.table.enter_scope();
        for statement in block.iter_mut() {
            match statement {
                BlockItem::Definition(definition) => self.process_definition(definition)?,
                BlockItem::Block(block) => self.process_block(block, return_void, in_while)?,
                BlockItem::Statement(statement) => match statement.as_mut() {
                    Statement::Expr(expr) => check_expr(expr, &self.table)?,
                    Statement::If {
                        condition,
                        then_block,
                        else_block,
                    } => {
                        check_expr(condition, &self.table)?;
                        self.process_block(then_block, return_void, in_while)?;
                        self.process_block(else_block, return_void, in_while)?;
                    }
                    Statement::While { condition, block } => {
                        check_expr(condition, &self.table)?;
                        self.process_block(block, return_void, true)?;
                    }
                    Statement::Return(expr) => match (expr, return_void) {
                        (None, true) => (),
                        (Some(_), true) | (None, false) => return Err(()),
                        (Some(expr), false) => check_expr(expr, &self.table)?,
                    },
                    Statement::Break | Statement::Continue => {
                        if !in_while {
                            return Err(());
                        }
                    }
                },
            }
        }
        self.table.exit_scope();
        Ok(())
    }

    pub fn check(&mut self, ast: &'a mut TranslationUnit) -> Result<(), ()> {
        for i in ast.iter_mut() {
            match i.as_mut() {
                GlobalItem::Definition(definition) => self.process_definition(definition)?,
                GlobalItem::FunctionDefinition {
                    return_void,
                    identifier,
                    parameter_list,
                    block,
                } => {
                    let parameter_type = todo!();
                    let return_type = if *return_void { Type::Void } else { Type::Int };
                    self.table
                        .insert_definition(identifier, SymbolTableItem::Function(return_type, parameter_type))?;

                    // 符号表中加入参数定义
                    self.table.enter_scope();

                    // 解析块
                    self.process_block(block, *return_void, false)?;
                    self.table.exit_scope();
                }
            }
        }
        Ok(())
    }
}
