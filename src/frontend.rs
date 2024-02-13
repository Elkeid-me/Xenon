mod ast;
mod checker;
mod expr;
mod front_optimizer;
mod parser;

fn generate_ast(code: &str) -> Result<ast::TranslationUnit, String> {
    let mut ast = parser::build_ast(code);
    checker::check(&mut ast)?;
    Ok(ast)
}

pub fn generate_ir(code: &str) -> Result<ast::TranslationUnit, String> {
    generate_ast(code)
}
