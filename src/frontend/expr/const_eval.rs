use crate::risk;

use super::{
    super::ast::{ArithmeticOp::*, ArithmeticUnaryOp::*, ConstInitListItem, Expr, InfixOp, InfixOp::*, UnaryOp::*},
    super::checker::*,
    types::Type::{self, Array, Int, Pointer},
};

use std::iter::zip;

// 类型, 是否合法, 是否是左值, 编译期计算值 (如果有)
// 这里, "左值" 的概念即 C 中的可修改左值 (SysY 中的 const 必须为编译期常量表达式)

type ReturnType<'a> = (Type<'a>, bool, Option<i32>);
impl<'a> Expr {
    fn infix_impl(lhs: &mut Expr, op: &InfixOp, rhs: &mut Expr, context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
        let (lhs_type, lhs_left_value, lhs_value) = lhs.const_eval_impl(context)?;
        let (rhs_type, _, rhs_value) = rhs.const_eval_impl(context)?;
        match op {
            Assign(_) => {
                if !lhs_left_value || !lhs_type.can_convert_to(&rhs_type) {
                    Err(format!("{0:?} 不是左值表达式，或 {1:?} 无法转换到 {0:?} 的类型", lhs, rhs))
                } else {
                    Ok((lhs_type, true, None))
                }
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
                    Err(format!("{:?} 或 {:?} 不是整数表达式", lhs, rhs))
                }
            }
            Logic(op) => match op {
                LogicalAnd => {
                    if matches!(lhs_value, Some(0)) || matches!(rhs_value, Some(0)) {
                        Ok((Int, false, Some(0)))
                    } else if let (Some(lhs), Some(rhs)) = (lhs_value, rhs_value) {
                        Ok((Int, false, Some((lhs != 0 && rhs != 0) as i32)))
                    } else if matches!((lhs_type, rhs_type), (Int, Int)) {
                        Ok((Int, false, None))
                    } else {
                        Err(format!("{:?} 或 {:?} 不是整数表达式", lhs, rhs))
                    }
                }
                LogicalOr => {
                    if matches!(lhs_value, Some(v) if v != 0) || matches!(rhs_value, Some(v) if v != 0) {
                        Ok((Int, false, Some(1)))
                    } else if let (Some(lhs), Some(rhs)) = (lhs_value, rhs_value) {
                        Ok((Int, false, Some((lhs != 0 || rhs != 0) as i32)))
                    } else if matches!((lhs_type, rhs_type), (Int, Int)) {
                        Ok((Int, false, None))
                    } else {
                        Err(format!("{:?} 或 {:?} 不是整数表达式", lhs, rhs))
                    }
                }
            },
        }
    }

    fn array_elem_impl(identifier: &String, subscripts: &mut Vec<Expr>, context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
        match context.search(identifier) {
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
                    Ok((Array(&lengths[subscripts.len()..]), false, None))
                }
            }
            Some(SymbolTableItem::ConstArray(lengths, init_list)) => {
                if subscripts.len() != lengths.len() {
                    return Err(format!("{:?} 错误", subscripts));
                }
                for expr in subscripts.iter_mut() {
                    if !matches!(expr.expr_type(context)?, Int) {
                        return Err(format!("{:?} 不是整型表达式", expr));
                    }
                }
                // if subscripts.len() == lengths.len() {
                // 好像多做了什么东西（
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
                // } else {
                //     Ok((Array(&lengths[subscripts.len()..]), false, None))
                // }
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
                    Ok((Pointer(&lengths[subscripts.len()..]), false, None))
                }
            }
            _ => Err(format!("{:?} 不能使用下标运算符", identifier)),
        }
    }

    fn const_eval_impl(&mut self, context: &'a SymbolTable) -> Result<ReturnType<'a>, String> {
        match self {
            Expr::InfixExpr(lhs, op, rhs) => Expr::infix_impl(lhs, op, rhs, context),
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
                    Others(_) => todo!(),
                }
            }
            Expr::Num(val) => Ok((Int, false, Some(*val))),
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
                        if !expr.expr_type(context)?.can_convert_to(expect_type) {
                            return Err(format!("{:?} 无法转换到类型 {:?}", expr, expect_type));
                        }
                    }
                    Ok((*type_, false, None))
                }
                _ => Err(format!("{} 不存在，或不是函数", identifier)),
            },
            Expr::ArrayElement(identifier, subscripts) => Expr::array_elem_impl(identifier, subscripts, context),
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
