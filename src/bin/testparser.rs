use std::fs;
use std::fmt;
use std::collections::HashMap;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "../vcl.pest"]
pub struct VclParser;

fn main() {
    let vcl_str = fs::read_to_string("/home/martin/vcl-parser/test.vcl").expect("Could not read VCL");
    let test = VclParser::parse(Rule::vcl, &vcl_str).expect("Could not parse VCL");
    println!("{}", test);
}
