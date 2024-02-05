use super::{
    ast::*,
    expr::{
        const_eval::{check_expr, const_eval},
        types::Type,
    },
};
use std::{collections::HashMap, vec};
pub enum SymbolTableItem {
    ConstVariable(i32),
    Variable,
    ConstArray(usize, Vec<i32>),
    Array(usize),
    Function(Type, Vec<Type>),
    Pointer,
}

pub type SymbolTable<'a> = Vec<HashMap<&'a str, SymbolTableItem>>;

pub trait Scope<'a> {
    fn new() -> Self;
    fn search(&self, identifier: &str) -> Option<&SymbolTableItem>;

    fn insert_definition(&mut self, identifier: &'a str, symbol: SymbolTableItem) -> Result<(), String>;

    fn enter_scope(&mut self);
    fn exit_scope(&mut self);
}

impl<'a> Scope<'a> for SymbolTable<'a> {
    fn new() -> Self {
        vec![HashMap::new()]
    }

    fn search(&self, identifier: &str) -> Option<&SymbolTableItem> {
        for map in self.iter().rev() {
            if let Some(info) = map.get(identifier) {
                return Some(info);
            }
        }
        return None;
    }

    fn insert_definition(&mut self, identifier: &'a str, symbol: SymbolTableItem) -> Result<(), String> {
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
            table: vec![HashMap::new()],
        }
    }

    fn process_definition(&mut self, definition: &'a mut Definition) -> Result<(), String> {
        match definition {
            Definition::ConstVariableDefinition(identifier, init) => match const_eval(init, &self.table) {
                Ok((_, _, Some(i))) => self.table.insert_definition(identifier, SymbolTableItem::ConstVariable(i)),
                _ => Err(format!("{:?} 不是常量表达式", init)),
            },
            Definition::ConstArrayDefinition {
                identifier,
                length,
                init_list,
            } => {
                for expr in init_list.iter_mut() {
                    if let None = const_eval(expr, &self.table)?.2 {
                        return Err(format!("{:?} 不是常量表达式", expr));
                    }
                }
                let init_list: Vec<i32> = init_list
                    .iter()
                    .map(|p| if let Expr::Num(i) = p { *i } else { unreachable!() })
                    .collect();
                if let Some(length) = const_eval(length, &self.table)?.2 {
                    let length = length as usize;
                    if (length) < init_list.len() {
                        return Err(format!("数组长度 {} 小于初始化列表长度 {}", length, init_list.len()));
                    }
                    self.table
                        .insert_definition(identifier, SymbolTableItem::ConstArray(length, init_list))
                } else {
                    Err(format!("{:?} 不是常量表达式", length))
                }
            }
            Definition::VariableDefinition(identifier, init) => {
                if let Some(expr) = init {
                    let type_ = const_eval(expr, &self.table)?.0;
                    if !matches!(type_, Type::Int) {
                        return Err(format!("{:?} 不是整型表达式", expr));
                    }
                }
                self.table.insert_definition(identifier, SymbolTableItem::Variable)
            }
            Definition::ArrayDefinition {
                identifier,
                length,
                init_list,
            } => {
                if let Some(length) = const_eval(length, &self.table)?.2 {
                    let length = length as usize;
                    if let Some(init_list) = init_list {
                        for expr in init_list.iter_mut() {
                            let type_ = const_eval(expr, &self.table)?.0;
                            if !matches!(type_, Type::Int) {
                                return Err(format!("{:?} 不是整型表达式", expr));
                            }
                        }
                        if length < init_list.len() {
                            return Err(format!("数组长度 {} 小于初始化列表长度 {}", length, init_list.len()));
                        }
                    }
                    self.table.insert_definition(identifier, SymbolTableItem::Array(length))
                } else {
                    Err(format!("{:?} 不是常量表达式", length))
                }
            }
        }
    }

    fn process_block(&mut self, block: &'a mut Block, return_void: bool, in_while: bool) -> Result<(), String> {
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
                        (Some(_), true) | (None, false) => return Err("return 语句返回表达式的类型与函数定义不匹配".to_string()),
                        (Some(expr), false) => {
                            let (type_, _, _) = const_eval(expr, &self.table)?;
                            if !matches!(type_, Type::Int) {
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
                    // for p in parameter_list.iter_mut() {
                    //     if let Parameter::Pointer(_, exprs) = p {
                    //         for expr in exprs.iter_mut() {
                    //             if let None = const_eval(expr, &self.table)?.2 {
                    //                 return Err(());
                    //             }
                    //         }
                    //     }
                    // }
                    // let parameter_type = parameter_list
                    //     .iter()
                    //     .map(|p| match p {
                    //         Parameter::Int(_) => Type::Int,
                    //         Parameter::Pointer(_, lengths) => Type::Pointer(Box::new(Type::Int)),
                    //     })
                    //     .collect();
                    let parameter_type = parameter_list
                        .iter()
                        .map(|p| match p {
                            Parameter::Int(_) => Type::Int,
                            Parameter::Pointer(_) => Type::Pointer,
                        })
                        .collect();
                    let return_type = if *return_void { Type::Void } else { Type::Int };
                    self.table
                        .insert_definition(identifier, SymbolTableItem::Function(return_type, parameter_type))?;
                    // 符号表中加入参数定义
                    self.table.enter_scope();
                    for p in parameter_list.iter() {
                        match p {
                            Parameter::Int(identifier) => self.table.insert_definition(identifier, SymbolTableItem::Variable)?,
                            Parameter::Pointer(identifier) => self.table.insert_definition(identifier, SymbolTableItem::Pointer)?,
                        }
                    }
                    // 解析块
                    self.process_block(block, *return_void, false)?;
                    self.table.exit_scope();
                }
            }
        }
        Ok(())
    }
}
