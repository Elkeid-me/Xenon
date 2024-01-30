use std::{fs::read_to_string, io::Write};

mod arg_parse;
mod frontend;
mod preprocessor;

fn compile() {
    todo!()
}
fn main() {
    let mut fs = std::fs::File::create("test/result.txt").unwrap();
    write!(
        &mut fs,
        "{:#?}",
        frontend::generate_ast(&preprocessor::preprocess(
            &std::fs::read_to_string("test/src.txt").unwrap()
        ))
    );
}
