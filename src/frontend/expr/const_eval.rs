use crate::risk;

use super::{
    super::ast::{ArithmeticOp::*, ArithmeticUnaryOp::*, ConstInitListItem, Expr, InfixOp::*, UnaryOp::*},
    super::checker::*,
    types::Type::{self, Array, Int, Pointer},
};

use std::iter::zip;

// 类型, 是否合法, 是否是左值, 编译期计算值 (如果有)
// 这里, "左值" 的概念即 C 中的可修改左值 (SysY 中的 const 必须为编译期常量表达式)
impl<'a> Expr {
    fn const_eval_impl(&mut self, context: &'a SymbolTable) -> Result<(Type<'a>, bool, Option<i32>), String> {
        match self {
            Expr::InfixExpr(lhs, op, rhs) => {
                let (lhs_type, lhs_left_value, lhs_value) = lhs.const_eval_impl(context)?;
                let (rhs_type, _, rhs_value) = rhs.const_eval_impl(context)?;
                match op {
                    Assign(_) => {
                        if !lhs_left_value || !lhs_type.can_convert_to(&rhs_type) {
                            return Err(format!("{0:?} 不是左值表达式，或 {1:?} 无法转换到 {0:?} 的类型", lhs, rhs));
                        }
                        Ok((lhs_type, true, None))
                    }
                    Arith(op) => {
                        if let (Some(lhs), Some(rhs)) = (lhs_value, rhs_value) {
                            let val = match op {
                                Multiply => lhs * rhs,
                                Divide => lhs / rhs,
                                Modulus => lhs % rhs,
                                Add => lhs + rhs,
                                Subtract => lhs - rhs,
                                LogicalAnd => (lhs != 0 && rhs != 0).into(),
                                LogicalOr => (lhs != 0 || rhs != 0).into(),
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
                        } else if matches!((lhs_type, rhs_type), (Int, Int)) {
                            Ok((Int, false, None))
                        } else {
                            Err(format!("{0:?} 或 {1:?} 不是整数表达式", lhs, rhs))
                        }
                    }
                }
            }
            Expr::UnaryExpr(op, exp) => {
                let (exp_type, _, exp_value) = exp.const_eval_impl(context)?;
                match op {
                    ArithUnary(op) => {
                        if let Some(i) = exp_value {
                            let value = match op {
                                LogicalNot => (i == 0).into(),
                                Negative => -i,
                                Positive => i,
                                BitNot => !i,
                            };
                            Ok((Int, false, Some(value)))
                        } else if matches!(exp_type, Int) {
                            Ok((Int, false, None))
                        } else {
                            Err(format!("{:?} 不是整数表达式", exp))
                        }
                    }
                    Other(_) => todo!(),
                }
            }
            Expr::Num(val) => Ok((Int, false, Some(*val))),
            // 一个孤立的标识符只有三种可能：
            //  - 整型
            //  - 数组
            // 而只有整型常量才能编译期计算.
            //
            // 整型常量必定带有初始化列表, 而初始化列表已经检查过并计算过了, 所以一定是 `InitializerListItem::Expr(Expr::Num(_))`
            Expr::Identifier(identifier) => match context.search(identifier) {
                Some(SymbolTableItem::ConstVariable(i)) => Ok((Int, false, Some(*i))),
                Some(SymbolTableItem::Variable) => Ok((Int, true, None)),
                Some(SymbolTableItem::Array(lengths)) | Some(SymbolTableItem::ConstArray(lengths, _)) => {
                    Ok((Array(lengths), false, None))
                }
                Some(SymbolTableItem::Pointer(lengths)) => Ok((Type::Pointer(lengths), false, None)),
                _ => Err(format!("{} 不存在，或不是整型、数组或指针变量", identifier)),
            },
            Expr::FunctionCall(identifier, arg_list) => match context.search(identifier) {
                Some(SymbolTableItem::Function(type_, para_types)) => {
                    if arg_list.len() != para_types.len() {
                        return Err("实参列表长度与函数定义不匹配".to_string());
                    }
                    for (expr, expect_type) in zip(arg_list.iter_mut(), para_types.iter()) {
                        let (expr_type, _, _) = expr.const_eval_impl(context)?;
                        if !expr_type.can_convert_to(&expect_type) {
                            return Err(format!("{:?} 无法转换到类型 {:?}", expr, expect_type));
                        }
                    }
                    Ok((*type_, false, None))
                }
                _ => Err(format!("{} 不存在，或不是函数", identifier)),
            },
            Expr::ArrayElement(identifier, subscripts) => match context.search(identifier) {
                Some(SymbolTableItem::Array(lengths)) => {
                    if subscripts.len() > lengths.len() {
                        return Err(format!("{:?} 错误", subscripts));
                    }
                    for expr in subscripts.iter_mut() {
                        if !matches!(expr.expr_type(context)?, Int) {
                            return Err(format!("{:?} 不是整型表达式", expr));
                        }
                    }
                    if subscripts.len() == lengths.len() {
                        Ok((Int, true, None))
                    } else {
                        Ok((Array(&lengths[lengths.len() - subscripts.len()..]), false, None))
                    }
                }
                Some(SymbolTableItem::ConstArray(lengths, init_list)) => {
                    if subscripts.len() > lengths.len() {
                        return Err(format!("{:?} 错误", subscripts));
                    }
                    for expr in subscripts.iter_mut() {
                        if !matches!(expr.expr_type(context)?, Int) {
                            return Err(format!("{:?} 不是整型表达式", expr));
                        }
                    }
                    if subscripts.len() == lengths.len() {
                        if subscripts.iter().all(|p| matches!(p, Expr::Num(_))) {
                            if !zip(subscripts.iter(), lengths.iter()).all(|(l, &r)| risk!(l, Expr::Num(i) => *i as usize) < r) {
                                return Err("下标超出范围".to_string());
                            }
                            let mut v_ref = *init_list;
                            for expr in subscripts.iter().take(subscripts.len() - 1) {
                                let i = risk!(expr, Expr::Num(i) => *i as usize);
                                if i >= v_ref.len() {
                                    return Ok((Int, false, Some(0)));
                                }
                                v_ref = risk!(&v_ref[i], ConstInitListItem::InitList(l) => l);
                            }

                            let i = risk!(subscripts.last().unwrap(), Expr::Num(i) => *i as usize);
                            if i >= v_ref.len() {
                                Ok((Int, false, Some(0)))
                            } else {
                                Ok((Int, false, Some(risk!(v_ref[i], ConstInitListItem::Num(i) => i))))
                            }
                        } else {
                            Ok((Int, false, None))
                        }
                    } else {
                        Ok((Array(&lengths[lengths.len() - subscripts.len()..]), false, None))
                    }
                }
                Some(SymbolTableItem::Pointer(lengths)) => {
                    if subscripts.len() - 1 > lengths.len() {
                        return Err(format!("{:?} 错误", subscripts));
                    }
                    for expr in subscripts.iter_mut() {
                        if !matches!(expr.expr_type(context)?, Int) {
                            return Err(format!("{:?} 不是整型表达式", expr));
                        }
                    }
                    if subscripts.len() - 1 == lengths.len() {
                        Ok((Int, true, None))
                    } else {
                        Ok((Pointer(&lengths[lengths.len() - subscripts.len()..]), false, None))
                    }
                }
                _ => Err(format!("{:?} 不能使用下标运算符", identifier)),
            },
        }
    }

    fn const_eval_wrap(&mut self, context: &'a SymbolTable) -> Result<(Type<'a>, bool, Option<i32>), String> {
        let (type_, is_left_value, value) = self.const_eval_impl(context)?;
        if let Some(i) = value {
            *self = Expr::Num(i);
        }
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
