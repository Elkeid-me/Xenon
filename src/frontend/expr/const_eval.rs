use super::super::ast::{ArithmeticOp::*, ArithmeticUnaryOp::*, ConstInitListItem, Expr, ExprInner};
use super::super::ast::{InfixOp, InfixOp::*, LogicOp::*, OtherUnaryOp::*, SimpleType, UnaryOp, UnaryOp::*};
use super::super::checker::*;
use super::types::Type::{self, Int, Pointer};
use crate::risk;

use std::{cmp::Ordering, iter::zip, mem::take};

// 类型, 是否合法, 是否是左值, 编译期计算值 (如果有)
// 这里, "左值" 的概念即 C 中的可修改左值 (SysY 中的 const 必须为编译期常量表达式)
type ReturnType<'a> = (Type<'a>, bool, Option<i32>);

fn __infix_impl<'a>(lhs: &mut Expr, op: &InfixOp, rhs: &mut Expr, context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
    let (lhs_type, lhs_left_value, lhs_value) = lhs.const_eval_wrap(context)?;
    let (rhs_type, _, rhs_value) = rhs.const_eval_wrap(context)?;
    match op {
        Assign(_) => {
            if !lhs_left_value || !rhs_type.can_convert_to(&lhs_type) {
                Err(format!("{0:?} 不是左值表达式，或 {1:?} 无法转换到 {0:?} 的类型", lhs, rhs))
            } else {
                Ok((lhs_type, true, None))
            }
        }
        Arith(op) => match (lhs_type, lhs_value, rhs_type, rhs_value) {
            (_, Some(lhs), _, Some(rhs)) => {
                let val = match op {
                    Multiply => lhs * rhs,
                    Divide => lhs / rhs,
                    Modulus => lhs % rhs,
                    Add => lhs + rhs,
                    Subtract => lhs - rhs,
                    BitLeftShift => lhs << rhs,
                    BitRightShift => lhs >> rhs,
                    BirXor => lhs ^ rhs,
                    BitAnd => lhs & rhs,
                    BitOr => lhs | rhs,
                    Equal => (lhs == rhs).into(),
                    NotEqual => (lhs != rhs).into(),
                    Greater => (lhs > rhs).into(),
                    GreaterOrEqual => (lhs >= rhs).into(),
                    Less => (lhs < rhs).into(),
                    LessOrEqual => (lhs <= rhs).into(),
                };
                Ok((Int, false, Some(val)))
            }
            (Int, _, Int, _) => Ok((Int, false, None)),
            _ => Err(format!("{:?} 或 {:?} 不是整数表达式", lhs, rhs)),
        },
        Logic(op) => match op {
            LogicalAnd => match (lhs_type, lhs_value, rhs_type, rhs_value) {
                (_, Some(lhs), _, Some(rhs)) => Ok((Int, false, Some((lhs != 0 && rhs != 0) as i32))),
                (_, Some(value), Int, None) | (Int, None, Int, Some(value)) if value == 0 => Ok((Int, false, Some(0))),
                (Int, _, Int, _) => Ok((Int, false, None)),
                _ => Err(format!("{:?} 或 {:?} 不是整数表达式", lhs, rhs)),
            },
            LogicalOr => match (lhs_type, lhs_value, rhs_type, rhs_value) {
                (_, Some(lhs), _, Some(rhs)) => Ok((Int, false, Some((lhs != 0 || rhs != 0) as i32))),
                (_, Some(value), Int, None) | (Int, None, Int, Some(value)) if value != 0 => Ok((Int, false, Some(0))),
                (Int, _, Int, _) => Ok((Int, false, None)),
                _ => Err(format!("{:?} 或 {:?} 不是整数表达式", lhs, rhs)),
            },
        },
    }
}

fn __elem_impl<'a>(subscripts: &mut [Expr], lengths: &'a [usize], context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
    for expr in subscripts.iter_mut() {
        if !matches!(expr.expr_type(context)?, Int) {
            return Err(format!("{:?} 不是整型表达式", expr));
        }
    }
    match (subscripts.len() - 1).cmp(&lengths.len()) {
        Ordering::Less => Ok((Pointer(&lengths[subscripts.len()..]), false, None)),
        Ordering::Equal => Ok((Int, true, None)),
        Ordering::Greater => Err(format!("{:?} 错误", subscripts)),
    }
}

fn __array_impl<'a>(identifier: &String, subscripts: &mut [Expr], context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
    match context.search(identifier) {
        Some(SymbolTableItem::Array(lengths)) => __elem_impl(subscripts, &lengths[1..], context),
        Some(SymbolTableItem::Pointer(lengths)) => __elem_impl(subscripts, lengths, context),
        Some(SymbolTableItem::ConstArray(lengths, init_list)) => {
            if subscripts.len() != lengths.len() {
                return Err(format!("{:?} 错误", subscripts));
            }
            for expr in subscripts.iter_mut() {
                if !matches!(expr.expr_type(context)?, Int) {
                    return Err(format!("{:?} 不是整型表达式", expr));
                }
            }
            if !subscripts.iter().all(|p| matches!(p.inner, ExprInner::Num(_))) {
                Ok((Int, false, None))
            } else {
                if !zip(subscripts.iter(), lengths.iter()).all(|(l, &r)| risk!(l.inner, ExprInner::Num(i) => i as usize) < r) {
                    return Err("下标超出范围".to_string());
                }
                let mut v_ref = *init_list;
                for expr in subscripts.iter().take(subscripts.len() - 1) {
                    let i = risk!(expr.inner, ExprInner::Num(i) => i as usize);
                    if i >= v_ref.len() {
                        return Ok((Int, false, Some(0)));
                    }
                    v_ref = risk!(&v_ref[i], ConstInitListItem::InitList(l) => l);
                }

                let i = risk!(subscripts.last().unwrap().inner, ExprInner::Num(i) => i as usize);
                if i >= v_ref.len() {
                    Ok((Int, false, Some(0)))
                } else {
                    Ok((Int, false, Some(risk!(v_ref[i], ConstInitListItem::Num(i) => i))))
                }
            }
        }
        _ => Err(format!("{:?} 不能使用下标运算符", identifier)),
    }
}

fn __unary_impl<'a>(expr: &mut Expr, op: &UnaryOp, context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
    let (expr_type, is_left_value, expr_value) = expr.const_eval_wrap(context)?;
    match op {
        ArithUnary(op) => match (expr_type, expr_value) {
            (_, Some(i)) => {
                let value = match op {
                    LogicalNot => (i == 0).into(),
                    Negative => -i,
                    Positive => i,
                    BitNot => !i,
                };
                Ok((Int, false, Some(value)))
            }
            (Int, None) => Ok((Int, false, None)),
            _ => Err(format!("{:?} 不是整数表达式", expr)),
        },
        Others(PostfixSelfIncrease) | Others(PostfixSelfDecrease) => match (expr_type, is_left_value) {
            (Int, true) => Ok((Int, false, None)),
            _ => Err(format!("{:?} 不是左值整型表达式", expr)),
        },
        Others(PrefixSelfIncrease) | Others(PrefixSelfDecrease) => match (expr_type, is_left_value) {
            (Int, true) => Ok((Int, true, None)),
            _ => Err(format!("{:?} 不是左值整型表达式", expr)),
        },
    }
}

impl<'a> Expr {
    fn __const_eval_impl(&mut self, context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
        match &mut self.inner {
            ExprInner::InfixExpr(lhs, op, rhs) => __infix_impl(lhs, op, rhs, context),
            ExprInner::UnaryExpr(op, expr) => __unary_impl(expr, op, context),
            ExprInner::Num(val) => Ok((Int, false, Some(*val))),
            ExprInner::Identifier(id) => match context.search(id) {
                Some(SymbolTableItem::ConstVariable(i)) => Ok((Int, false, Some(*i))),
                Some(SymbolTableItem::Variable) => Ok((Int, true, None)),
                Some(SymbolTableItem::Array(lengths)) => Ok((Pointer(&lengths[1..]), false, None)),
                Some(SymbolTableItem::ConstArray(_, _)) => Err(format!("常量数组 {} 不能转为指针", id)),
                Some(SymbolTableItem::Pointer(lengths)) => Ok((Type::Pointer(lengths), false, None)),
                _ => Err(format!("{} 不存在，或不是整型、数组或指针变量", id)),
            },
            ExprInner::FunctionCall(id, arg_list) => match context.search(id) {
                Some(SymbolTableItem::Function(type_, para_types)) => {
                    if arg_list.len() != para_types.len() {
                        return Err("实参列表长度与函数定义不匹配".to_string());
                    }
                    for (expr, expect_type) in zip(arg_list.iter_mut(), para_types.iter()) {
                        if !expr.expr_type(context)?.can_convert_to(expect_type) {
                            return Err(format!("{:?} 无法转换到类型 {:?}", expr, expect_type));
                        }
                    }
                    Ok((*type_, false, None))
                }
                _ => Err(format!("{} 不存在，或不是函数", id)),
            },
            ExprInner::ArrayElement(identifier, subscripts) => __array_impl(identifier, subscripts, context),
        }
    }

    fn const_eval_wrap(&mut self, context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
        let (type_, is_left_value, value) = self.__const_eval_impl(context)?;
        if let Some(i) = value {
            self.inner = ExprInner::Num(i);
        }
        if let ExprInner::UnaryExpr(UnaryOp::ArithUnary(Positive), expr) = &mut self.inner {
            *self = take(expr);
        }
        self.type_ = match type_ {
            Int => SimpleType::Int,
            Type::Void => SimpleType::Void,
            Pointer(_) => SimpleType::Pointer,
        };
        Ok((type_, is_left_value, value))
    }

    pub fn check_expr(&mut self, context: &SymbolTable) -> Result<(), String> {
        self.const_eval_wrap(context)?;
        Ok(())
    }

    pub fn expr_type(&mut self, context: &'a SymbolTable) -> Result<Type<'a>, String> {
        let (type_, _, _) = self.const_eval_wrap(context)?;
        Ok(type_)
    }

    pub fn const_eval(&mut self, context: &SymbolTable) -> Result<i32, String> {
        match self.const_eval_wrap(context)?.2 {
            Some(i) => Ok(i),
            None => Err(format!("{:?} 不是常量表达式", self)),
        }
    }
}
