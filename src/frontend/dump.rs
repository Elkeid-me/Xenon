use std::mem::take;

use super::ast::{
    ArithmeticOp::*, ArithmeticUnaryOp::*, AssignOp::*, ExprInner::*, InfixOp::*, OtherUnaryOp::*, SimpleType::*, UnaryOp::*, *,
};

struct Counter {
    value: i32,
}

impl Counter {
    fn get(&mut self) -> i32 {
        self.value += 1;
        self.value
    }
}

fn dump_array_elem_lvalue(counter: &mut Counter, id: &String, lengths: &Vec<Expr>) -> (String, String) {
    let mut last_id = id.clone();
    let str = lengths
        .iter()
        .map(|expr| {
            let (exp_str, exp_id) = dump_expr_rvalue(counter, expr);
            let old_id = take(&mut last_id);
            last_id = format!("%{}", counter.get());
            format!("{}{} = getelemptr {}, {}\n", exp_str, &last_id, old_id, exp_id)
        })
        .collect();
    (str, last_id)
}

fn dump_array_elem_rvalue(counter: &mut Counter, id: &String, lengths: &Vec<Expr>, type_: SimpleType) -> (String, String) {
    match type_ {
        Int => {
            let (expr_str, expr_id) = dump_array_elem_lvalue(counter, id, lengths);
            let id = format!("%{}", counter.get());
            (format!("{}{} = load {}\n", expr_str, id, expr_id), id)
        }
        Pointer => dump_array_elem_lvalue(counter, id, lengths),
        _ => unreachable!(),
    }
}

fn dump_function_rvalue(counter: &mut Counter, id: &String, args: &Vec<Expr>, type_: SimpleType) -> (String, String) {
    let (arg_str, arg_ids) = args
        .iter()
        .map(|expr| dump_expr_rvalue(counter, expr))
        .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{}{}", l_str, r_str), format!("{}, {}", l_id, r_id)))
        .unwrap_or_default();
    match type_ {
        Int => {
            let tmp_id = counter.get();
            (format!("{}%{} = call @{}({})\n", arg_str, tmp_id, id, arg_ids), format!("%{}", tmp_id))
        }
        Void => ((format!("{}call @{}({})\n", arg_str, id, arg_ids)), "".to_string()),
        _ => unreachable!(),
    }
}

fn dump_expr_rvalue(counter: &mut Counter, expr: &Expr) -> (String, String) {
    match &expr.inner {
        InfixExpr(_, Assign(op), _) => {
            let (exp_str, exp_id) = dump_expr_lvalue(counter, expr);
            let id = format!("%{}", counter.get());
            (format!("{}{} = load {}\n", exp_str, id, exp_id), id)
        }
        InfixExpr(lhs, Logic(op), rhs) => todo!(),
        InfixExpr(lhs, Arith(op), rhs) => {
            let op_name = match op {
                Multiply => "mul",
                Divide => "div",
                Modulus => "mod",
                Add => "add",
                Subtract => "sub",
                BitLeftShift => "shl",
                BitRightShift => "sar",
                BirXor => "xor",
                BitAnd => "and",
                BitOr => "or",
                Equal => "eq",
                NotEqual => "ne",
                Greater => "gt",
                GreaterOrEqual => "ge",
                Less => "lt",
                LessOrEqual => "le",
            };
            let (lhs_str, lhs_id) = dump_expr_rvalue(counter, lhs);
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            let id = format!("%{}", counter.get());
            (format!("{}{}{} = {} {}, {}\n", lhs_str, rhs_str, id, op_name, lhs_id, rhs_id), id)
        }
        UnaryExpr(op, exp) => todo!(),
        Num(i) => ("".to_string(), i.to_string()),
        Identifier(id) => match expr.type_ {
            Int => {
                let tmp_id = format!("%{}", counter.get());
                (format!("{} = load %{}\n", tmp_id, id), tmp_id)
            }
            Pointer => ("".to_string(), format!("%{}", id)),
            _ => unreachable!(),
        },
        FunctionCall(id, args) => dump_function_rvalue(counter, id, args, expr.type_),
        ArrayElement(id, lengths) => dump_array_elem_rvalue(counter, id, lengths, expr.type_),
    }
}

fn dump_expr_lvalue(counter: &mut Counter, expr: &Expr) -> (String, String) {
    match &expr.inner {
        InfixExpr(lhs, Assign(Assignment), rhs) => {
            let (lhs_str, lhs_id) = dump_expr_lvalue(counter, lhs);
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            (format!("{}{}store {}, {}\n", lhs_str, rhs_str, lhs_id, rhs_id), lhs_id)
        }
        InfixExpr(lhs, Assign(op), rhs) => {
            let (lhs_str, lhs_id) = dump_expr_lvalue(counter, lhs);
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            (format!("{}{}store {}, {}\n", lhs_str, rhs_str, lhs_id, rhs_id), lhs_id)
        }
        UnaryExpr(Others(PrefixSelfIncrease), expr) => {
            let (expr_str, expr_id) = dump_expr_lvalue(counter, expr);
            (format!("{} = add {}, 1\nstore {}, {}", expr_str, expr_id, expr_id, expr_id), expr_id)
        }
        UnaryExpr(Others(PrefixSelfDecrease), expr) => {
            let (expr_str, expr_id) = dump_expr_lvalue(counter, expr);
            (format!("{} = sub {}, 1\nstore {}, {}", expr_str, expr_id, expr_id, expr_id), expr_id)
        }
        Identifier(id) => ("".to_string(), format!("%{}", id)),
        ArrayElement(id, lengths) => dump_array_elem_lvalue(counter, id, lengths),
        _ => unreachable!(),
    }
}

// fn dump_expr_statement(counter: &mut Counter, expr: &Expr) -> String {
//     match &expr.inner {
//         InfixExpr(_, Arith(_), _)
//         | InfixExpr(_, Logic(_), _)
//         | UnaryExpr(ArithUnary(_), _)
//         | Num(_)
//         | Identifier(_)
//         | ArrayElement(_, _) => "".to_string(),
//         InfixExpr(_, Assign(_), _) => dump_expr_lvalue(counter, expr).0,
//         UnaryExpr(Others(PrefixSelfDecrease), expr)
//         | UnaryExpr(Others(PostfixSelfDecrease), expr) => todo!(),
//         UnaryExpr(Others(PrefixSelfIncrease), expr)
//         | UnaryExpr(Others(PostfixSelfIncrease), expr) => todo!(),
//         FunctionCall(id, args) => todo!(),
//         _ => todo!(),
//     }
// }

fn dump_statement(counter: &mut Counter, statement: &Statement) -> String {
    match statement {
        Statement::Expr(expr) => dump_expr_rvalue(counter, expr).0,
        Statement::If {
            condition,
            then_block,
            else_block,
        } => todo!(),
        Statement::While { condition, block } => todo!(),
        Statement::Return(expr) => match expr {
            Some(expr) => {
                let (expr_str, expr_id) = dump_expr_rvalue(counter, expr);
                format!("{}ret {}\n", expr_str, expr_id)
            }
            None => "ret\n".to_string(),
        },
        Statement::Break => todo!(),
        Statement::Continue => todo!(),
    }
}

fn dump_def(counter: &mut Counter, def: &Definition) -> String {
    match def {
        Definition::VariableDef(id, init) => match init {
            Some(expr) => {
                let (expr_str, expr_id) = dump_expr_rvalue(counter, expr);
                format!("{}%{} = alloc i32\nstore %{}, {}\n", expr_str, id, id, expr_id)
            }
            None => format!("%{} = alloc i32\n", id),
        },
        Definition::ArrayDef { id, lengths, init_list } => "un impl \n".to_string(),
        Definition::ConstArrayDef { id, lengths, init_list } => "un impl \n".to_string(),
        _ => "".to_string(),
    }
}

fn dump_block(counter: &mut Counter, block: &Block) -> (String, String) {
    let id = format!("%{}", counter.get());
    let body: String = block
        .iter()
        .map(|item| match item {
            BlockItem::Def(def) => dump_def(counter, def),
            BlockItem::Block(block) => "un impl \n".to_string(), // format!("{}\n", dump_block(counter, block)),
            BlockItem::Statement(statement) => dump_statement(counter, statement),
        })
        .collect();
    (body, id)
}

fn dump_function_def(
    counter: &mut Counter,
    return_void: bool,
    id: &String,
    parameter_list: &Vec<Parameter>,
    block: &Block,
) -> String {
    fn point_type_str(lengths: &[usize]) -> String {
        lengths
            .iter()
            .fold("i32".to_string(), |state, len| format!("[{}, {}]", state, len))
    }
    let para_str = parameter_list
        .iter()
        .map(|parameter| match parameter {
            Parameter::Int(id) => format!("@{}: i32", id),
            Parameter::Pointer(id, lengths) => format!("@{}: *{}", id, point_type_str(lengths)),
            _ => unreachable!(),
        })
        .reduce(|l, r| format!("{}, {}", l, r))
        .unwrap_or_default();

    let entry_id = format!("%{}", counter.get());
    let para_alloc: String = parameter_list
        .iter()
        .map(|parameter| match parameter {
            Parameter::Int(id) => format!("%{} = alloc i32\nstore @{}, %{}\n", id, id, id),
            Parameter::Pointer(id, lengths) => format!("%{} = alloc *{}\nstore @{}, %{}\n", id, point_type_str(lengths), id, id),
            _ => unreachable!(),
        })
        .collect();
    let return_type_str = if return_void { ": i32" } else { "" };
    let (block, _) = dump_block(counter, block);
    format!("fun @{}({}){}{{\n{}:\n{}{}}}\n", id, para_str, return_type_str, entry_id, para_alloc, block)
}

pub fn dump_ir(ast: &TranslationUnit) -> String {
    let mut counter = Counter { value: 0 };
    let prelude = r"
decl @getint(): i32
decl @getch(): i32
decl @getarray(*i32): i32
decl @putint(i32): i32
decl @putch(i32): i32
decl @putarray(i32, *i32): i32
decl @starttime(): i32
decl @stoptime(): i32
";
    let ir: String = ast
        .iter()
        .map(|p| match p.as_ref() {
            GlobalItem::Def(def) => dump_def(&mut counter, def),
            GlobalItem::FuncDef {
                return_void,
                id,
                parameter_list,
                block,
            } => dump_function_def(&mut counter, *return_void, id, parameter_list, block),
        })
        .collect();
    format!("{}{}", prelude, ir)
}
