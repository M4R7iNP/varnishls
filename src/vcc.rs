use std::{iter::Iterator, iter::Peekable, str::SplitTerminator};

use crate::varnish_builtins::*;

// https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vmodtool.py#L890
fn tokenize(txt: &str, seps: Option<&str>, quotes: Option<&str>) -> Vec<String> {
    let mut seps_str = String::from("[](){},=");
    if let Some(s) = seps {
        seps_str = s.to_string();
    }
    let mut quotes_str = String::from("'\"");
    if let Some(q) = quotes {
        quotes_str = q.to_string();
    }

    let mut quote: Option<char> = None;
    let mut out: Vec<String> = Vec::new();
    let mut inside = false;

    for c in txt.chars() {
        if let Some(q) = quote {
            if c == q {
                inside = false;
                quote = None;
                let last = out.last_mut().unwrap();
                last.push(c);
            } else {
                let last = out.last_mut().unwrap();
                last.push(c);
            }
        } else if c.is_whitespace() {
            inside = false;
        } else if seps_str.contains(c) {
            inside = false;
            out.push(c.to_string());
        } else if quotes_str.contains(c) {
            quote = Some(c);
            out.push(c.to_string());
        } else if inside {
            let last = out.last_mut().unwrap();
            last.push(c);
        } else {
            out.push(c.to_string());
            inside = true;
        }
    }

    out
}

fn parse_doc(lines_iter: &mut SplitTerminator<&str>) -> String {
    lines_iter
        // .take_while(|double_line| *line != "SEE ALSO" && *line != "ACKNOWLEDGEMENTS")
        .take_while(|double_line| {
            // stop if this chunk is a doc header (e.g. https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvmod_std/vmod.vcc#L441-L442)
            double_line.lines().all(|line| {
                line.is_empty()
                    || line
                        .chars()
                        .fold(0, |count, char| count + (char == '=') as usize)
                        != line.len()
            })
        })
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
}

fn parse_func<'a>(
    toks: &mut Peekable<impl Iterator<Item = &'a String>>,
    lines_iter: &mut SplitTerminator<&str>,
) -> Func {
    let mut func = Func {
        ..Default::default()
    };
    let _type = toks.next().unwrap();
    let name = toks.next().unwrap();
    func.name = name.to_owned();
    assert_eq!(toks.next(), Some(&"(".to_string()), "expected arguments");
    let signature_tokens = toks
        .take_while(|tok| tok.as_str() != ")")
        .map(|tok| tok.as_str())
        .collect::<Vec<_>>();
    let signature = signature_tokens.join(" ");
    func.signature = Some(signature);
    func.ret_type = Some(_type.to_string());
    println!("hohoho: {:?}", parse_arguments(signature_tokens));
    let doc = parse_doc(lines_iter);
    func.doc = Some(doc);
    func
}

fn parse_arguments(toks: Vec<&str>) -> Vec<FuncArg> {
    let mut toks_iter = toks.iter().peekable();
    let mut args: Vec<FuncArg> = vec![];
    while let Some(tok) = toks_iter.next() {
        if *tok == ")" {
            break;
        }

        let mut optional = false;
        let mut type_str = tok;
        if *tok == "[" {
            optional = true;
            // type_str = toks_iter.next().unwrap();
            type_str = toks_iter.next().unwrap();
        }

        let r#type = match *type_str {
            "STRING" => Type::String,
            "STRING_LIST" => Type::String,
            "BOOL" => Type::Bool,
            "INT" => Type::Number,
            "REAL" => Type::Number,
            "DURATION" => Type::Duration,
            "BLOB" => Type::Blob,
            "BACKEND" => Type::Backend,
            "ENUM" => {
                // TODO: consume enum values
                let mut values = vec![];
                let next_tok = toks_iter.peek();
                if next_tok.is_some() && **next_tok.unwrap() == "{" {
                    loop {
                        toks_iter.next(); // consume token (either , or {)

                        let next_tok = toks_iter.next().unwrap();

                        if *next_tok != "}" {
                            values.push(next_tok.to_string());
                        }

                        let next_tok = toks_iter.peek();
                        if next_tok.is_none() || **next_tok.unwrap() != "," {
                            break;
                        }
                    }
                    toks_iter.next(); // consume }
                }
                Type::Enum(values)
            }
            "PRIV_TASK" => {
                // consume until , or )
                toks_iter.next_if(|tok| **tok == ",");
                continue;
            }
            _ => {
                todo!("NOT IMPLEMENTED: {}", type_str);
            }
        };

        let name = toks_iter
            .next_if(|tok| **tok != ",")
            .unwrap_or(type_str)
            .to_string();
        let default_value: Option<String> = {
            let next_tok = toks_iter.peek();
            if next_tok == Some(&&"=") {
                toks_iter.next(); // consume
                Some(toks_iter.next().unwrap().to_string())
            } else {
                None
            }
        };

        let func_arg = FuncArg {
            name,
            optional,
            r#type,
            default_value,
        };

        args.push(func_arg);

        if optional {
            assert_eq!(
                toks_iter.next(),
                Some(&"]"),
                "expected end of optional argument"
            );
        }

        if toks_iter.peek() == Some(&&",") {
            toks_iter.next();
        }
    }

    args
}

/**
 * Parse a vmod vcc file, using the varnish vmodtool.py tokenizer, and collect all functions,
 * objects and object methods.
 *
 * This includes the documentation for the function/method.
 */
pub fn parse_vcc(vcc_file: String) -> Type {
    let vcc_file_with_starting_newline = format!("\n{}", vcc_file);
    let mut parts = vcc_file_with_starting_newline.split("\n$").peekable();
    parts.next(); // remove first part

    let mut scope = Obj {
        read_only: true,
        ..Default::default()
    };

    while let Some(s) = parts.next() {
        let mut lines_iter = s.split_terminator("\n\n");
        let first_line = lines_iter.next().unwrap();
        let tok_vec = tokenize(first_line, None, None);
        let mut toks = tok_vec.iter().peekable();
        println!("hello: ({:?})", toks);
        match toks.next().unwrap().as_str() {
            "Module" => {
                scope.name = toks.next().unwrap().to_string();
            }
            "ABI" => {}
            "Function" => {
                println!("func line: {}", first_line);
                let func = parse_func(&mut toks, &mut lines_iter);
                scope
                    .properties
                    .insert(func.name.to_owned(), Type::Func(func));
            }
            "Object" => {
                let name = toks.next().unwrap();
                let mut obj = Obj {
                    name: name.to_owned(),
                    ..Default::default()
                };
                // take all following methods and put into obj
                while let Some(ps) = parts.peek() {
                    let mut lines_iter = ps.split_terminator("\n\n");
                    let first_line = lines_iter.next().unwrap();
                    println!("line: {}", first_line);
                    let tok_vec = tokenize(first_line, None, None);
                    let mut toks = tok_vec.iter().peekable();

                    if toks.next().unwrap() != "Method" {
                        break;
                    }

                    // advance iterator
                    parts.next().unwrap();
                    let func = parse_func(&mut toks, &mut lines_iter);
                    obj.properties
                        .insert(func.name.to_owned(), Type::Func(func));
                }
                let func = Func {
                    name: name.to_owned(),
                    r#return: Some(Box::new(Type::Obj(obj))),
                    ..Default::default()
                };
                scope.properties.insert(name.to_owned(), Type::Func(func));
            }
            _ => {}
        }
    }

    Type::Obj(scope)
}
