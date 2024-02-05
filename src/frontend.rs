mod ast;
mod checker;
mod expr;
mod front_optimizer;
mod parser;

pub struct Frontend {}

pub fn generate_ast(code: &str) -> ast::TranslationUnit {
    let mut ast = parser::AstBuilder::new().build_ast(code);
    if let Err(s) = checker::Checker::new().check(&mut ast) {
        println!("{}", s);
    }
    ast
}

pub fn generate_ir(code: &str) -> koopa::ir::Program {
    todo!()
}
