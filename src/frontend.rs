mod ast;
mod checker;
mod expr;
mod parser;

fn generate_ast(code: &str) -> Result<ast::TranslationUnit, String> {
    checker::check(parser::build_ast(code))
}

pub fn generate_ir(code: &str) -> Result<ast::TranslationUnit, String> {
    generate_ast(code)
}
