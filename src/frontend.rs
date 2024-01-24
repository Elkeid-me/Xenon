use pest::Parser;
use pest_derive::Parser;

mod ast;

#[derive(Parser)]
#[grammar = "frontend/sysy.pest"]
struct SYSYParser;

pub fn generate_ir(code: &String) -> koopa::ir::Program {
    todo!()
}
