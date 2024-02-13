use super::{
    ast::{Definition::*, *},
    expr::types::Type::{self, *},
};
use std::{collections::HashMap, mem::take, vec};
pub enum SymbolTableItem<'a> {
    ConstVariable(i32),
    Variable,
    ConstArray(&'a Vec<usize>, &'a ConstInitList),
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
        None
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

trait InitListTrait {
    fn new_list(l: Vec<Self>) -> Self
    where
        Self: Sized;
    fn new_item(expr: &mut Expr, symbol_table: &SymbolTable) -> Result<Self, String>
    where
        Self: Sized;
    fn get_last(v: &mut Vec<Self>) -> &mut Vec<Self>
    where
        Self: Sized;
}

impl InitListTrait for ConstInitListItem {
    fn new_list(l: Vec<Self>) -> Self {
        Self::InitList(Box::new(l))
    }
    fn new_item(expr: &mut Expr, symbol_table: &SymbolTable) -> Result<Self, String> {
        Ok(Self::Num(expr.const_eval(symbol_table)?))
    }
    fn get_last(v: &mut Vec<Self>) -> &mut Vec<Self> {
        risk!(v.last_mut().unwrap(), Self::InitList(l) => l.as_mut())
    }
}

impl InitListTrait for InitListItem {
    fn new_list(l: Vec<Self>) -> Self {
        Self::InitList(Box::new(l))
    }
    fn new_item(expr: &mut Expr, symbol_table: &SymbolTable) -> Result<Self, String> {
        if !matches!(expr.expr_type(symbol_table)?, Int) {
            Err(format!("{:?} 不是整型表达式", expr))
        } else {
            Ok(Self::Expr(take(expr)))
        }
    }
    fn get_last(v: &mut Vec<Self>) -> &mut Vec<Self> {
        risk!(v.last_mut().unwrap(), Self::InitList(l) => l.as_mut())
    }
}

fn __impl<T>(context: &SymbolTable, init_list: &mut InitList, len_prod: &[usize]) -> Result<(Vec<T>, usize), String>
where
    T: InitListTrait,
{
    if init_list.is_empty() {
        return Ok((Vec::<T>::new(), *len_prod.last().unwrap()));
    }
    let mut v = Vec::<T>::new();
    let mut sum = 0usize;
    for ele in init_list {
        match ele {
            InitListItem::InitList(l) => {
                if len_prod.len() == 1 || sum % len_prod[0] != 0 {
                    return Err(format!("{:?} 不能是初始化列表", l));
                }
                let mut z = 0usize;
                for i in 0..len_prod.len() - 1 {
                    if sum % len_prod[i] == 0 {
                        z = i;
                    } else {
                        break;
                    }
                }
                let (l, s) = __impl(context, l, &len_prod[0..=z])?;
                let mut v_ref = &mut v;
                for _ in z + 2..len_prod.len() {
                    if v_ref.is_empty() {
                        v_ref.push(T::new_list(Vec::new()));
                    }
                    v_ref = T::get_last(v_ref);
                }
                v_ref.push(T::new_list(l));
                sum += s;
            }
            InitListItem::Expr(expr) => {
                let mut v_ref = &mut v;
                for &i in len_prod.iter().rev().skip(1) {
                    if v_ref.is_empty() || sum % i == 0 {
                        v_ref.push(T::new_list(Vec::new()))
                    }
                    v_ref = T::get_last(v_ref);
                }
                v_ref.push(T::new_item(expr, context)?);
                sum += 1;
            }
        }
        if sum > *len_prod.last().unwrap() {
            return Err("初始化列表过长".to_string());
        }
    }
    Ok((v, *len_prod.last().unwrap()))
}

fn process_init_list<T>(context: &SymbolTable, init_list: &mut InitList, lengths: &[usize]) -> Result<Vec<T>, String>
where
    T: InitListTrait,
{
    let len_prod: Vec<usize> = lengths
        .iter()
        .rev()
        .scan(1, |l, &r| {
            *l = *l * r;
            Some(*l)
        })
        .collect();
    Ok(__impl::<T>(context, init_list, &len_prod)?.0)
}

fn process_definition<'a>(context: &mut SymbolTable<'a>, def: &'a mut Definition) -> Result<(), String> {
    match def {
        ConstVariableDefTmp(identifier, init) => {
            *def = ConstVariableDef(take(identifier), init.const_eval(context)?);
            let (identifier, init) = risk!(def, ConstVariableDef(id, i) => (id, *i));
            context.insert_definition(identifier, ConstVariable(init))
        }
        ConstArrayDefTmp { id, lengths, init_list } => {
            for expr in lengths.iter_mut() {
                if expr.const_eval(context)? <= 0 {
                    return Err(format!("{:?} 的值小于等于 0", expr));
                }
            }
            let lengths: Vec<usize> = lengths.iter_mut().map(|p| risk!(p, Expr::Num(i) => *i as usize)).collect();
            let init_list = process_init_list(context, init_list, &lengths)?;
            *def = ConstArrayDef {
                id: take(id),
                lengths,
                init_list,
            };
            let (identifier, lengths, init_list) = risk!(def, ConstArrayDef { id, lengths, init_list } => (id, lengths, init_list));
            context.insert_definition(identifier, ConstArray(lengths, init_list))
        }
        VariableDef(identifier, init) => {
            if let Some(expr) = init {
                if !matches!(expr.expr_type(context)?, Int) {
                    return Err(format!("{:?} 不是整型表达式", expr));
                }
            }
            context.insert_definition(identifier, Variable)
        }
        ArrayDefTmp { id, lengths, init_list } => {
            for expr in lengths.iter_mut() {
                expr.const_eval(context)?;
            }
            let lengths: Vec<usize> = lengths.iter_mut().map(|p| risk!(p, Expr::Num(i) => *i as usize)).collect();
            let init_list = match init_list {
                Some(init_list) => Some(process_init_list(context, init_list, &lengths)?),
                None => None,
            };
            *def = ArrayDef {
                id: take(id),
                lengths,
                init_list,
            };
            let (identifier, lengths) = risk!(def, ArrayDef { id, lengths, init_list: _ } => (id, lengths));
            context.insert_definition(identifier, Array(lengths))
        }
        _ => unreachable!(),
    }
}

fn process_block<'a>(context: &mut SymbolTable<'a>, block: &'a mut Block, return_void: bool, in_while: bool) -> Result<(), String> {
    context.enter_scope();
    for block_item in block.iter_mut() {
        match block_item {
            BlockItem::Def(definition) => process_definition(context, definition)?,
            BlockItem::Block(block) => process_block(context, block, return_void, in_while)?,
            BlockItem::Statement(statement) => match statement.as_mut() {
                Statement::Expr(expr) => expr.check_expr(context)?,
                Statement::If {
                    condition,
                    then_block,
                    else_block,
                } => match condition.expr_type(context)? {
                    Void => return Err(format!("{:?} 不能作为 if 的条件", condition)),
                    _ => {
                        process_block(context, then_block, return_void, in_while)?;
                        process_block(context, else_block, return_void, in_while)?;
                    }
                },
                Statement::While { condition, block } => match condition.expr_type(context)? {
                    Void => return Err(format!("{:?} 不能作为 if 的条件", condition)),
                    _ => process_block(context, block, return_void, true)?,
                },
                Statement::Return(expr) => match (expr, return_void) {
                    (None, true) => (),
                    (None, false) => return Err("int 函数中的 return 语句未返回表达式".to_string()),
                    (Some(expr), true) => return Err(format!("在 void 函数中返回了表达式 {:?}", expr)),
                    (Some(expr), false) => {
                        if !matches!(expr.expr_type(context)?, Int) {
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
    context.exit_scope();
    Ok(())
}

pub fn check(ast: &mut TranslationUnit) -> Result<(), String> {
    let mut context = vec![HashMap::from([
        ("getint", Function(Int, Vec::new())),
        ("getch", Function(Int, Vec::new())),
        ("getarray", Function(Int, vec![Pointer(&[])])),
        ("putint", Function(Void, vec![Int])),
        ("putch", Function(Void, vec![Int])),
        ("putarray", Function(Int, vec![Int, Pointer(&[])])),
        ("starttime", Function(Void, Vec::new())),
        ("stoptime", Function(Void, Vec::new())),
    ])];
    for i in ast.iter_mut() {
        match i.as_mut() {
            GlobalItem::Def(definition) => process_definition(&mut context, definition)?,
            GlobalItem::FuncDef {
                return_void,
                id,
                parameter_list,
                block,
            } => {
                for p in parameter_list.iter_mut() {
                    if let Parameter::PointerTmp(id, exprs) = p {
                        for expr in exprs.iter_mut() {
                            expr.const_eval(&context)?;
                        }
                        *p = Parameter::Pointer(
                            take(id),
                            exprs.iter().map(|p| risk!(p, Expr::Num(i) => *i as usize)).collect(),
                        )
                    }
                }
                let parameter_type = parameter_list
                    .iter()
                    .map(|p| match p {
                        Parameter::Int(_) => Int,
                        Parameter::Pointer(_, lengths) => Pointer(lengths),
                        Parameter::PointerTmp(_, _) => unreachable!(),
                    })
                    .collect();
                let return_type = if *return_void { Void } else { Int };
                context.insert_definition(id, Function(return_type, parameter_type))?;
                context.enter_scope();
                for p in parameter_list.iter() {
                    match p {
                        Parameter::Int(identifier) => context.insert_definition(identifier, Variable)?,
                        Parameter::Pointer(identifier, lengths) => {
                            context.insert_definition(identifier, SymbolTableItem::Pointer(lengths))?
                        }
                        Parameter::PointerTmp(_, _) => unreachable!(),
                    }
                }
                process_block(&mut context, block, *return_void, false)?;
                context.exit_scope();
            }
        }
    }
    Ok(())
}
