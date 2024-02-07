use super::{
    super::ast::{ArithmeticOp::*, ArithmeticUnaryOp::*, Expr, InfixOp::*, UnaryOp::*},
    super::checker::*,
    types::Type::{self, Array, Int},
};

use std::iter::zip;

// 类型, 是否合法, 是否是左值, 编译期计算值 (如果有)
// 这里, "左值" 的概念即 C 中的可修改左值 (SysY 中的 const 必须为编译期常量表达式)
fn const_eval_impl(expr: &mut Expr, context: &SymbolTable) -> Result<(Type, bool, Option<i32>), String> {
    match expr {
        Expr::InfixExpr(lhs, op, rhs) => {
            let (lhs_type, lhs_left_value, lhs_value) = const_eval_impl(lhs, context)?;
            let (rhs_type, _, rhs_value) = const_eval_impl(rhs, context)?;
            match op {
                Assign(_) => {
                    if !lhs_left_value || !lhs_type.can_convert_to(&rhs_type) {
                        return Err(format!("{0:?} 不是左值表达式，或 {1:?} 无法转换到 {0:?} 的类型", lhs, rhs,));
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
                        *expr = Expr::Num(val);
                        Ok((Int, false, Some(val)))
                    } else if matches!((lhs_type, rhs_type), (Int, Int)) {
                        Ok((Int, false, None))
                    } else {
                        Err(format!("{0:?} 或 {1:?} 不是整数表达式", lhs, rhs))
                    }
                }
            }
        }
        Expr::UnaryExpr(op, e) => {
            let (exp_type, _, exp_value) = const_eval_impl(e, context)?;
            match op {
                ArithUnary(op) => {
                    if let Some(i) = exp_value {
                        let value = match op {
                            LogicalNot => (i == 0).into(),
                            Negative => -i,
                            Positive => i,
                            BitNot => !i,
                        };
                        *expr = Expr::Num(value);
                        Ok((Int, false, Some(value)))
                    } else if matches!(exp_type, Int) {
                        Ok((Int, false, None))
                    } else {
                        Err(format!("{:?} 不是整数表达式", e))
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
            Some(SymbolTableItem::ConstVariable(i)) => {
                *expr = Expr::Num(*i);
                Ok((Int, false, Some(*i)))
            }
            Some(SymbolTableItem::Variable) => Ok((Int, true, None)),
            Some(SymbolTableItem::Array(length)) | Some(SymbolTableItem::ConstArray(length, _)) => {
                Ok((Array(length.clone()), false, None))
            }
            _ => Err(format!("{} 不存在，或不是整型、数组或指针变量", identifier)),
        },
        Expr::FunctionCall(identifier, arg_list) => match context.search(identifier) {
            Some(SymbolTableItem::Function(type_, para_types)) => {
                if arg_list.len() != para_types.len() {
                    return Err("实参列表长度与函数定义不匹配".to_string());
                }
                for (expr, expect_type) in zip(arg_list.iter_mut(), para_types.iter()) {
                    let (expr_type, _, _) = const_eval_impl(expr, context)?;
                    if !expr_type.can_convert_to(&expect_type) {
                        return Err(format!("{:?} 无法转换到类型 {:?}", expr, expect_type));
                    }
                }
                Ok((type_.clone(), false, None))
            }
            _ => Err(format!("{} 不存在，或不是函数", identifier)),
        },
        Expr::ArrayElement(identifier, length) => {
            todo!()
        }
    }
}

pub fn check_expr(expr: &mut Expr, context: &SymbolTable) -> Result<(), String> {
    const_eval_impl(expr, context)?;
    Ok(())
}

pub fn expr_type(expr: &mut Expr, context: &SymbolTable) -> Result<Type, String> {
    let (type_, _, _) = const_eval_impl(expr, context)?;
    Ok(type_)
}

pub fn const_eval(expr: &mut Expr, context: &SymbolTable) -> Result<Option<i32>, String> {
    Ok(const_eval_impl(expr, context)?.2)
}
