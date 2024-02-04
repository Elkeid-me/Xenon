use super::{
    super::ast::{ArithmeticOp::*, Expr, InfixOp::*, InitializerListItem, UnaryOp},
    super::checker::*,
    types::Type,
};

// 类型, 是否合法, 是否是左值, 编译期计算值 (如果有)
// 这里, "左值" 的概念即 C 中的可修改左值 (SysY 中的 const 必须为编译期常量表达式)
pub fn const_eval(expr: &mut Expr, context: &SymbolTable) -> Result<(Type, bool, Option<i32>), ()> {
    match expr {
        Expr::InfixExpr(lhs, op, rhs) => {
            let (lhs_type, lhs_left_value, lhs_value) = const_eval(lhs, context)?;
            let (rhs_type, rhs_left_value, rhs_value) = const_eval(rhs, context)?;
            match op {
                Assign(op) => {
                    if !lhs_left_value {
                        return Err(());
                    }
                    todo!()
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
                        Ok((Type::Int, false, Some(val)))
                    } else if matches!((lhs_type, rhs_type), (Type::Int, Type::Int)) {
                        Ok((Type::Int, false, None))
                    } else {
                        Err(())
                    }
                }
            }
        }
        Expr::UnaryExpr(op, rhs) => {
            let (exp_type, exp_left_value, exp_value) = const_eval(rhs, context)?;
            match op {
                UnaryOp::PostfixSelfIncrease
                | UnaryOp::PostfixSelfDecrease
                | UnaryOp::PrefixSelfIncrease
                | UnaryOp::PrefixSelfDecrease => {
                    if !exp_left_value || !matches!(exp_type, Type::Int) {
                        // 看起来暂时不需要数组的自增自减
                        Err(())
                    } else {
                        Ok((Type::Int, false, None)) // C 标准中, 前缀自增/自减运算符为右值
                    }
                }
                UnaryOp::LogicalNot => todo!(),
                UnaryOp::Negative => todo!(),
                UnaryOp::Positive => todo!(),
                UnaryOp::BitNot => todo!(),
                UnaryOp::AddressOf => todo!(),
                UnaryOp::Indirection => todo!(),
            }
        }
        Expr::Num(val) => Ok((Type::Int, false, Some(*val))),
        // 一个孤立的标识符只有三种可能：
        //  - 整型
        //  - 数组
        // 而只有整型常量才能编译期计算.
        //
        // 整型常量必定带有初始化列表, 而初始化列表已经检查过并计算过了, 所以一定是 `InitializerListItem::Expr(Expr::Num(_))`
        Expr::Identifier(identifier) => match context.search(identifier) {
            Some(SymbolTableItem::ConstVariable(i)) => {
                *expr = Expr::Num(*i);
                Ok((Type::Int, false, Some(*i)))
            }
            Some(SymbolTableItem::Variable(_)) => Ok((Type::Int, true, None)),
            _ => Err(()),
        },
        Expr::FunctionCall(identifier, arg_list) => match context.search(identifier) {
            Some(SymbolTableItem::Function(type_, para_types)) => {
                for (expr, expect_type) in arg_list.iter_mut().zip(para_types) {
                    let (expr_type, _, _) = const_eval(expr, context)?;
                    if !expr_type.can_convert_to(type_) {
                        return Err(());
                    }
                }
                Ok((type_.clone(), false, None))
            }
            _ => Err(()),
        },
        Expr::ArrayElement(identifier, length) => todo!(),
    }
}

pub fn check_expr(expr: &mut Expr, context: &SymbolTable) -> Result<(), ()> {
    const_eval(expr, context)?;
    Ok(())
}
