mod ast;
mod front_optimizer;
mod parser;

pub struct Frontend {}

pub fn generate_ast(code: &str) -> ast::TranslationUnit {
    parser::AstBuilder::new().build_ast(code)
}

pub fn generate_ir(code: &str) -> koopa::ir::Program {
    todo!()
}
