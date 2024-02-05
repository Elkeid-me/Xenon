use std::{
    fs::{read_to_string, File},
    io::Write,
};

mod arg_parse;
mod frontend;
mod preprocessor;

fn compile() {
    todo!()
}

fn main() -> std::io::Result<()> {
    match frontend::generate_ir(&preprocessor::preprocess(&read_to_string("test/src.txt")?)) {
        Ok(ast) => {
            let mut fs = File::create("test/result.txt")?;
            fs.write_fmt(format_args!("{:#?}", ast))?
        }
        Err(s) => println!("{}", s),
    }
    Ok(())
}
