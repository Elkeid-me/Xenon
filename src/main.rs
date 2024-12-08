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

use std::fs::{read_to_string, File};
use std::io::Write;

mod arg_parse;
mod frontend;
mod preprocessor;

fn compile() -> Result<(), Box<dyn std::error::Error>> {
    let (mode, input, output) = arg_parse::parse(std::env::args())?;
    let code = preprocessor::preprocess(&read_to_string(input)?.replace("\r\n", "\n"));
    let ir = frontend::generate_ir(&code)?;
    let mut f = File::create(output)?;
    f.write_fmt(format_args!("{}", ir))?;
    Ok(())
}

fn main() {
    if let Err(s) = compile() {
        println!("{}", s);
        std::process::exit(0);
    }
}
