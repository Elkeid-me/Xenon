mod ast;
mod checker;
mod dump;
mod expr;
mod parser;

fn generate_ast(code: &str) -> Result<ast::TranslationUnit, String> {
    checker::check(parser::build_ast(code))
}

pub fn generate_ir(code: &str) -> Result<String, String> {
    Ok(dump::dump_ir(&generate_ast(code)?))
}
