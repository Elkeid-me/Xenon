// Copyright (C) 2024 Elkeid-me
//
// This file is part of Xenon.
//
// Xenon is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Xenon is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Xenon.  If not, see <http://www.gnu.org/licenses/>.

use super::ast::{ArithmeticOp::*, ArithmeticUnaryOp::*, AssignOp::*, ExprInner::*, InfixOp::*, LogicOp::*, OtherUnaryOp::*};
use super::ast::{SimpleType::*, UnaryOp::*, *};
use std::mem::take;

struct Counter {
    value: i32,
}

impl Counter {
    fn get(&mut self) -> String {
        self.value += 1;
        format!("%{}", self.value)
    }
}

fn dump_array_elem_lvalue(counter: &mut Counter, id: &String, subscripts: &Vec<Expr>, id_is_pointer: bool) -> (String, String) {
    let mut last_id = counter.get();
    let mut old_id = format!("%{}", id);
    let (exp_str, exp_id) = dump_expr_rvalue(counter, &subscripts[0]);
    let first_str = if id_is_pointer {
        let str_1 = format!("{}    {} = load {}\n", exp_str, last_id, old_id);
        old_id = take(&mut last_id);
        last_id = counter.get();
        format!("{}    {} = getptr {}\n", str_1, last_id, old_id)
    } else {
        format!("{}    {} = getelemptr {}, {}\n", exp_str, last_id, old_id, exp_id)
    };
    let str: String = subscripts
        .iter()
        .skip(1)
        .map(|expr| {
            let (exp_str, exp_id) = dump_expr_rvalue(counter, expr);
            let old_id = take(&mut last_id);
            last_id = counter.get();
            format!("{}    {} = getelemptr {}, {}\n", exp_str, last_id, old_id, exp_id)
        })
        .collect();
    (format!("{}{}", first_str, str), last_id)
}

fn dump_array_elem_rvalue(
    counter: &mut Counter,
    id: &String,
    subscripts: &Vec<Expr>,
    type_: SimpleType,
    id_is_pointer: bool,
) -> (String, String) {
    match type_ {
        Int => {
            let (expr_str, expr_id) = dump_array_elem_lvalue(counter, id, subscripts, id_is_pointer);
            let id = counter.get();
            (format!("{}    {} = load {}\n", expr_str, id, expr_id), id)
        }
        Pointer => dump_array_elem_lvalue(counter, id, subscripts, id_is_pointer),
        _ => unreachable!(),
    }
}

fn dump_expr_rvalue(counter: &mut Counter, expr: &Expr) -> (String, String) {
    match &expr.inner {
        InfixExpr(lhs, Assign(Assignment), rhs) => {
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            let (lhs_str, lhs_id) = dump_expr_lvalue(counter, lhs);
            (format!("{}{}    store {}, {}\n", rhs_str, lhs_str, rhs_id, lhs_id), rhs_id)
        }
        InfixExpr(lhs, Assign(_), rhs) => {
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            let (lhs_str, lhs_id) = dump_expr_lvalue(counter, lhs);
            (format!("{}{}    store {}, {}\n", rhs_str, lhs_str, rhs_id, lhs_id), rhs_id)
        }
        InfixExpr(lhs, Logic(LogicalAnd), rhs) => {
            let (lhs_str, lhs_id) = dump_expr_rvalue(counter, lhs);
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            let lhs_ne_0_id = counter.get();
            let rhs_ne_0_id = counter.get();
            let eval_rhs_id = counter.get();
            let expr_eq_0_id = counter.get();
            let expr_id = counter.get();
            let tmp_id = counter.get();
            let expr_store_id = counter.get();
            let next_id = counter.get();
            (
                format!(
                    r"{lhs_str}    {expr_store_id} = alloc i32
    {lhs_ne_0_id} = ne {lhs_id}, 0
    br {lhs_ne_0_id}, {eval_rhs_id}, {expr_eq_0_id}
{expr_eq_0_id}:
    store 0, {expr_store_id}
    jump {next_id}
{eval_rhs_id}:
{rhs_str}    {rhs_ne_0_id} = ne {rhs_id}, 0
    {tmp_id} = and {lhs_ne_0_id}, {rhs_ne_0_id}
    store {tmp_id}, {expr_store_id}
    jump {next_id}
{next_id}:
    {expr_id} = load {expr_store_id}
"
                ),
                expr_id,
            )
        }
        InfixExpr(lhs, Logic(LogicalOr), rhs) => {
            let (lhs_str, lhs_id) = dump_expr_rvalue(counter, lhs);
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            let lhs_ne_0_id = counter.get();
            let rhs_ne_0_id = counter.get();
            let eval_rhs_id = counter.get();
            let expr_eq_1_id = counter.get();
            let expr_id = counter.get();
            let tmp_id = counter.get();
            let expr_store_id = counter.get();
            let next_id = counter.get();
            (
                format!(
                    r"{lhs_str}    {expr_store_id} = alloc i32
    {lhs_ne_0_id} = ne {lhs_id}, 0
    br {lhs_ne_0_id}, {expr_eq_1_id}, {eval_rhs_id}
{expr_eq_1_id}:
    store 1, {expr_store_id}
    jump {next_id}
{eval_rhs_id}:
{rhs_str}    {rhs_ne_0_id} = ne {rhs_id}, 0
    {tmp_id} = or {lhs_ne_0_id}, {rhs_ne_0_id}
    store {tmp_id}, {expr_store_id}
    jump {next_id}
{next_id}:
    {expr_id} = load {expr_store_id}
"
                ),
                expr_id,
            )
        }
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
            let id = counter.get();
            (format!("{lhs_str}{rhs_str}    {id} = {op_name} {lhs_id}, {rhs_id}\n"), id)
        }
        UnaryExpr(Others(PrefixSelfIncrease), _) | UnaryExpr(Others(PrefixSelfDecrease), _) => {
            let (exp_str, exp_id) = dump_expr_lvalue(counter, expr);
            let id = counter.get();
            (format!("{}    {} = load {}\n", exp_str, id, exp_id), id)
        }
        UnaryExpr(Others(PostfixSelfIncrease), expr) => {
            let (expr_str, expr_id) = dump_expr_lvalue(counter, expr);
            let id_1 = counter.get();
            let id_2 = counter.get();
            (
                format!(
                    r"{expr_str}    {id_1} = load {expr_id}
    {id_2} = add {id_1}, 1
    store {id_2}, {expr_id}
"
                ),
                id_1,
            )
        }
        UnaryExpr(Others(PostfixSelfDecrease), expr) => {
            let (expr_str, expr_id) = dump_expr_lvalue(counter, expr);
            let id_1 = counter.get();
            let id_2 = counter.get();
            (
                format!(
                    r"{expr_str}    {id_1} = load {expr_id}
    {id_2} = sub {id_1}, 1
    store {id_2}, {expr_id}
"
                ),
                id_1,
            )
        }
        UnaryExpr(ArithUnary(BitNot), expr) => {
            let (expr_str, expr_id) = dump_expr_rvalue(counter, expr);
            let id = counter.get();
            (format!("{expr_str}    {id} = xor {expr_id}, 1\n"), id)
        }
        UnaryExpr(ArithUnary(LogicalNot), expr) => {
            let (expr_str, expr_id) = dump_expr_rvalue(counter, expr);
            let id = counter.get();
            (format!("{expr_str}    {id} = eq {expr_id}, 0\n"), id)
        }
        Num(i) => (String::new(), i.to_string()),
        Identifier(id) => match expr.type_ {
            Int => {
                let tmp_id = counter.get();
                (format!("    {} = load %{}\n", tmp_id, id), tmp_id)
            }
            Pointer => (String::new(), format!("%{}", id)),
            _ => unreachable!(),
        },
        FunctionCall(id, args) => {
            let (arg_str, arg_ids) = args
                .iter()
                .map(|expr| dump_expr_rvalue(counter, expr))
                .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{}{}", l_str, r_str), format!("{}, {}", l_id, r_id)))
                .unwrap_or_default();
            let tmp_id = counter.get();
            (format!("{}    {} = call @{}({})\n", arg_str, tmp_id, id, arg_ids), tmp_id)
        }
        ArrayElement(id, subscripts, id_is_pointer) => dump_array_elem_rvalue(counter, id, subscripts, expr.type_, *id_is_pointer),
        _ => unreachable!(),
    }
}

fn dump_expr_xvalue(counter: &mut Counter, expr: &Expr) -> String {
    match &expr.inner {
        InfixExpr(_, Assign(_), _) => dump_expr_lvalue(counter, expr).0,
        InfixExpr(lhs, Arith(_), rhs) => format!("{}{}", dump_expr_xvalue(counter, lhs), dump_expr_xvalue(counter, rhs)),
        InfixExpr(_, Logic(_), _) => dump_expr_rvalue(counter, expr).0,
        UnaryExpr(_, _) => todo!(),
        Num(_) => String::new(),
        Identifier(_) => String::new(),
        FunctionCall(id, args) => {
            let (arg_str, arg_ids) = args
                .iter()
                .map(|expr| dump_expr_rvalue(counter, expr))
                .reduce(|(l_str, l_id), (r_str, r_id)| (format!("{}{}", l_str, r_str), format!("{}, {}", l_id, r_id)))
                .unwrap_or_default();
            format!("{}    call @{}({})\n", arg_str, id, arg_ids)
        }
        ArrayElement(_, exprs, _) => exprs.iter().map(|expr| dump_expr_xvalue(counter, expr)).collect(),
    }
}

fn dump_expr_lvalue(counter: &mut Counter, expr: &Expr) -> (String, String) {
    match &expr.inner {
        InfixExpr(lhs, Assign(Assignment), rhs) => {
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            let (lhs_str, lhs_id) = dump_expr_lvalue(counter, lhs);
            (format!("{}{}    store {}, {}\n", rhs_str, lhs_str, rhs_id, lhs_id), lhs_id)
        }
        InfixExpr(lhs, Assign(_), rhs) => {
            let (rhs_str, rhs_id) = dump_expr_rvalue(counter, rhs);
            let (lhs_str, lhs_id) = dump_expr_lvalue(counter, lhs);
            (format!("{}{}    store {}, {}\n", rhs_str, lhs_str, rhs_id, lhs_id), lhs_id)
        }
        UnaryExpr(Others(PrefixSelfIncrease), expr) => {
            let (expr_str, expr_id) = dump_expr_lvalue(counter, expr);
            (expr_str, expr_id)
        }
        UnaryExpr(Others(PrefixSelfDecrease), expr) => {
            let (expr_str, expr_id) = dump_expr_lvalue(counter, expr);
            (expr_str, expr_id)
        }
        Identifier(id) => (String::new(), format!("%{}", id)),
        ArrayElement(id, subscripts, id_is_pointer) => dump_array_elem_lvalue(counter, id, subscripts, *id_is_pointer),
        _ => unreachable!(),
    }
}

fn dump_statement(counter: &mut Counter, statement: &Statement, while_id: &str, while_next_id: &str) -> String {
    match statement {
        Statement::Expr(expr) => dump_expr_xvalue(counter, expr),
        Statement::If {
            condition,
            then_block,
            else_block,
        } => {
            let next_block_id = counter.get();
            let (cond_str, cond_id) = dump_expr_rvalue(counter, condition);
            let (then_str, then_id) = dump_block(counter, then_block, while_id, while_next_id);
            if else_block.is_empty() {
                format!(
                    r"{cond_str}    br {cond_id}, {then_id}, {next_block_id}
{then_id}:
{then_str}    jump {next_block_id}
{next_block_id}:
"
                )
            } else {
                let (else_str, else_id) = dump_block(counter, else_block, while_id, while_next_id);
                format!(
                    r"{cond_str}    br {cond_id}, {then_id}, {else_id}
{then_id}:
{then_str}    jump {next_block_id}
{else_id}:
{else_str}    jump {next_block_id}
{next_block_id}:
"
                )
            }
        }
        Statement::While { condition, block } => {
            let while_id = counter.get();
            let while_next_id = counter.get();
            let (cond_str, cond_id) = dump_expr_rvalue(counter, condition);
            let (block_str, block_id) = dump_block(counter, block, &while_id, &while_next_id);
            format!(
                r"    jump {while_id}
{block_id}:
{block_str}    jump {while_id}
{while_id}:
{cond_str}    br {cond_id}, {block_id}, {while_next_id}
{while_next_id}:
"
            )
        }
        Statement::Return(expr) => match expr {
            Some(expr) => {
                let (expr_str, expr_id) = dump_expr_rvalue(counter, expr);
                format!("{}    ret {}\n", expr_str, expr_id)
            }
            None => "    ret\n".to_string(),
        },
        Statement::Break => format!("    jump {}\n", while_next_id),
        Statement::Continue => format!("    jump {}\n", while_id),
    }
}

fn dump_def(counter: &mut Counter, def: &Definition) -> String {
    match def {
        Definition::VariableDef(id, init) => match init {
            Some(expr) => {
                let (expr_str, expr_id) = dump_expr_rvalue(counter, expr);
                format!(
                    r"{expr_str}    %{id} = alloc i32
    store {expr_id}, %{id}
"
                )
            }
            None => format!("    %{} = alloc i32\n", id),
        },
        Definition::ArrayDef { id: _, lengths: _, init_list: _ } => "un impl \n".to_string(),
        Definition::ConstArrayDef { id: _, lengths: _, init_list: _ } => "un impl \n".to_string(),
        _ => String::new(),
    }
}

fn dump_global_def(counter: &mut Counter, def: &Definition) -> String {
    match def {
        Definition::VariableDef(id, init) => match init {
            Some(expr) => {
                let (_, expr_id) = dump_expr_rvalue(counter, expr);
                format!("global %{} = alloc i32, {}\n", id, expr_id)
            }
            None => format!("global %{} = alloc i32, 0\n", id),
        },
        Definition::ArrayDef { id: _, lengths: _, init_list: _ } => "un impl \n".to_string(),
        Definition::ConstArrayDef { id: _, lengths: _, init_list: _ } => "un impl \n".to_string(),
        _ => String::new(),
    }
}

fn dump_block(counter: &mut Counter, block: &Block, while_id: &str, while_next_id: &str) -> (String, String) {
    let id = counter.get();
    let body: String = block
        .iter()
        .map(|item| match item {
            BlockItem::Def(def) => dump_def(counter, def),
            BlockItem::Block(block) => format!("{}\n", dump_block(counter, block, while_id, while_next_id).0),
            BlockItem::Statement(statement) => dump_statement(counter, statement, while_id, while_next_id),
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
            .rev()
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

    let entry_id = counter.get();
    let para_alloc: String = parameter_list
        .iter()
        .map(|parameter| match parameter {
            Parameter::Int(id) => format!("%{} = alloc i32\nstore @{}, %{}\n", id, id, id),
            Parameter::Pointer(id, lengths) => format!("%{} = alloc *{}\n    store @{}, %{}\n", id, point_type_str(lengths), id, id),
            _ => unreachable!(),
        })
        .collect();
    let return_type_str = if return_void { "" } else { ": i32" };
    let (block, _) = dump_block(counter, block, "", "");
    format!("fun @{}({}){} {{\n{}:\n{}{}}}\n", id, para_str, return_type_str, entry_id, para_alloc, block)
}

pub fn dump_ir(ast: &TranslationUnit) -> String {
    let mut counter = Counter { value: 0 };
    let prelude = r"decl @getint(): i32
decl @getch(): i32
decl @getarray(*i32): i32
decl @putint(i32): i32
decl @putch(i32): i32
decl @putarray(i32, *i32): i32
decl @starttime(): i32
decl @stoptime(): i32";
    let ir: String = ast
        .iter()
        .map(|p| match p.as_ref() {
            GlobalItem::Def(def) => dump_global_def(&mut counter, def),
            GlobalItem::FuncDef {
                return_void,
                id,
                parameter_list,
                block,
            } => dump_function_def(&mut counter, *return_void, id, parameter_list, block),
        })
        .collect();
    let ir: Vec<&str> = ir.split('\n').filter(|s| !s.is_empty()).collect();
    let mut v = Vec::new();
    let mut flag = false;
    for i in ir {
        if i.starts_with("    jump") || i.starts_with("    ret") || i.starts_with("    br") {
            if !flag {
                flag = true;
                v.push(i);
            }
        } else {
            v.push(i);
            flag = false;
        }
    }
    let mut v_2 = Vec::new();
    for i in 0..v.len() - 1 {
        if (v[i].starts_with("    jump") || v[i].starts_with("    ret") || v[i].starts_with("    br"))
            && (v[i + 1].chars().last().unwrap() != ':' && v[i + 1].chars().last().unwrap() != '}')
        {
            v_2.push(format!("{}\n", v[i]));
            v_2.push(format!("{}:\n", counter.get()));
        } else {
            v_2.push(format!("{}\n", v[i]));
        }
    }
    v_2.push(format!("{}\n", v.last().unwrap()));
    let mut v_3: Vec<&str> = Vec::new();
    for i in 0..v_2.len() - 1 {
        if !(v_2[i].starts_with("    jump") || v_2[i].starts_with("    ret") || v_2[i].starts_with("    br"))
            && !v_2[i].ends_with("{\n")
            && (v_2[i + 1].ends_with("}\n") || v_2[i + 1].ends_with(":\n"))
        {
            v_3.push(&v_2[i]);
            v_3.push("    ret\n");
        } else {
            v_3.push(&v_2[i]);
        }
    }
    v_3.push(v_2.last().unwrap());
    let ir: String = v_3.into_iter().collect();
    format!("{}\n{}", prelude, ir)
}
