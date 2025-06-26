use std::iter::{Iterator, Peekable};
use std::str::SplitTerminator;

use crate::varnish_builtins::*;

// https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vmodtool.py#L890
/*
fn tokenize<'a>(txt: &'a str, seps: Option<&str>, quotes: Option<&str>) -> Vec<&'a str> {
    let mut seps_str = String::from("[](){},=");
    if let Some(s) = seps {
        seps_str = s.to_string();
    }
    let mut quotes_str = String::from("'\"");
    if let Some(q) = quotes {
        quotes_str = q.to_string();
    }

    let mut quote: Option<char> = None;
    let mut token_start = 0;
    let mut out: Vec<&str> = Vec::new();
    let mut inside = false;

    for (i, c) in txt.char_indices() {
        if let Some(q) = quote {
            if c == q {
                inside = false;
                quote = None;
                out.push(&txt[token_start..(i + 1)]);
            }
        } else if c.is_whitespace() {
            if inside {
                out.push(&txt[token_start..i]);
            }
            inside = false;
        } else if seps_str.contains(c) {
            if inside {
                out.push(&txt[token_start..i]);
            }
            inside = false;
            out.push(&txt[i..(i + 1)]);
        } else if quotes_str.contains(c) {
            if inside {
                out.push(&txt[token_start..i]);
            }
            quote = Some(c);
            inside = true;
            token_start = i;
        } else if !inside {
            token_start = i;
            inside = true;
        }
    }

    if inside {
        out.push(&txt[token_start..txt.len()]);
    }

    out
}
*/

struct Tokenizer<'a> {
    cursor: usize,
    txt: &'a str,
    seps_str: String,
    quotes_str: String,
}

impl<'a> Tokenizer<'a> {
    fn new(txt: &'a str) -> Self {
        Self {
            cursor: 0,
            txt,
            seps_str: String::from("[](){},="),
            quotes_str: String::from("'\""),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = &'a str;

    // https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vmodtool.py#L890
    fn next(&mut self) -> Option<Self::Item> {
        let mut quote: Option<char> = None;
        let mut token_start = 0;
        let mut inside = false;
        let slice = &self.txt[self.cursor..];

        macro_rules! emit_token {
            ($value:expr) => {
                Some(&slice[token_start..$value])
            }
        }

        for (i, c) in slice.char_indices() {
            self.cursor += 1;
            if let Some(q) = quote {
                if c == q {
                    return emit_token!(i + 1);
                }
            } else if c.is_whitespace() {
                if inside {
                    return emit_token!(i);
                }
            } else if self.seps_str.contains(c) {
                if inside {
                    self.cursor -= 1;
                    return emit_token!(i);
                }
                return Some(&slice[i..(i + 1)]);
            } else if self.quotes_str.contains(c) {
                if inside {
                    return emit_token!(i);
                }
                quote = Some(c);
                inside = true;
                token_start = i;
            } else if !inside {
                token_start = i;
                inside = true;
            }
        }

        emit_token!(slice.len()).filter(|s| !s.is_empty())
    }
}

fn parse_doc(lines_iter: &mut SplitTerminator<&str>) -> String {
    lines_iter
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

fn parse_type<'a>(toks: &mut Peekable<impl Iterator<Item = &'a str>>) -> Option<Type> {
    let type_str = toks.next().unwrap();
    match type_str {
        "STRING" => Some(Type::String),
        "STRING_LIST" => Some(Type::String),
        "REGEX" => Some(Type::String),
        "STRANDS" => Some(Type::String),
        "BOOL" => Some(Type::Bool),
        "INT" => Some(Type::Number),
        "REAL" => Some(Type::Number),
        "IP" => Some(Type::IP),
        "DURATION" => Some(Type::Duration),
        "TIME" => Some(Type::Time),
        "BYTES" => Some(Type::Bytes), 
        "BLOB" => Some(Type::Blob),
        "BACKEND" => Some(Type::Backend),
        "PROBE" => Some(Type::Probe),
        "ACL" => Some(Type::Acl),
        "HTTP" => Some(Type::Obj(Default::default())), // for now
        "HEADER" => Some(Type::String),
        "ENUM" => {
            let mut values = vec![];

            assert_eq!(toks.next(), Some("{"), "expected enum values");
            while let Some(tok) = toks.next() {
                values.push(tok.to_string());
                match toks.next() {
                    Some(",") => {
                        continue;
                    }
                    Some("}") => {
                        break;
                    }
                    Some(tok) => {
                        panic!("unexpected token «{tok}». expected either «,» or «}}»");
                    }
                    _ => {
                        panic!("unexpected end of enums");
                    }
                }
            }
            Some(Type::Enum(values))
        }
        "VOID" | "PRIV_TASK" | "PRIV_VCL" | "PRIV_CALL" | "STEVEDORE" => None,
        _ => {
            // todo!("NOT IMPLEMENTED: {type_str:?}");
            None
        }
    }
}

fn parse_func<'a>(
    toks: &mut Peekable<impl Iterator<Item = &'a str>>,
    lines_iter: &mut SplitTerminator<&str>,
) -> Func {
    let r#return = parse_type(toks).map(Box::new);

    let name = toks.next().unwrap().trim_start_matches('.').to_string();
    assert_eq!(toks.next(), Some("("), "expected arguments");

    let args = parse_func_args(toks);
    // assert_eq!(toks.next(), Some(")"), "expected end of arguments");
    let doc = parse_doc(lines_iter);

    Func {
        name,
        r#return,
        args,
        doc: Some(doc),
        ..Default::default()
    }
}

fn parse_func_args<'a>(toks: &mut Peekable<impl Iterator<Item = &'a str>>) -> Vec<FuncArg> {
    let mut arg_toks = toks.take_while(|tok| *tok != ")").peekable();
    let mut args: Vec<FuncArg> = vec![];
    while arg_toks.peek().is_some() {
        let optional = arg_toks.next_if(|tok| *tok == "[").is_some();
        let r#type = parse_type(&mut arg_toks);
        let name = arg_toks
            .next_if(|tok| *tok != ",")
            .map(|tok| tok.to_string());

        let default_value: Option<String> = arg_toks
            .next_if(|tok| *tok == "=")
            .map(|_| arg_toks.next().unwrap().to_string());

        // skip unrecognized types
        if r#type.is_some() {
            args.push(FuncArg {
                name,
                optional,
                r#type,
                default_value,
            });
        }

        if optional {
            assert_eq!(
                arg_toks.next(),
                Some("]"),
                "expected end of optional argument"
            );
        }

        match arg_toks.peek() {
            Some(&",") => {
                arg_toks.next();
                continue;
            }
            Some(&")") | None => break,
            Some(tok) => panic!("unexpected token {tok:?}"),
        }
    }

    args
}

fn check_restrict<'a>(parts: &mut Peekable<impl Iterator<Item = &'a str>>) -> Option<Vec<String>> {
    let next_part = parts.peek()?;
    if next_part.starts_with("Restrict ") {
        Some(
            parts.next().unwrap()["Restrict ".len()..]
                .trim()
                .split(char::is_whitespace)
                .map(|str| str.to_string())
                .collect(),
        )
    } else {
        None
    }
}

/**
 * Parse a vmod vcc file, using the varnish vmodtool.py tokenizer, and collect all functions,
 * objects and object methods.
 *
 * This includes the documentation for the function/method.
 */
pub fn parse_vcc(vcc_file: String) -> Type {
    // split file by newline+dollar sign
    let vcc_file_with_starting_newline = format!("\n{vcc_file}");
    let mut parts = vcc_file_with_starting_newline.split("\n$").peekable();
    parts.next(); // remove first part

    let mut scope = Obj {
        read_only: true,
        ..Default::default()
    };

    // for each part, split by double newline (statements can span multiple lines)
    while let Some(s) = parts.next() {
        let mut lines_iter = s.split_terminator("\n\n");
        let first_line = lines_iter.next().expect("No lines");
        // let tok_vec = tokenize(first_line, None, None);
        // let mut toks = tok_vec.iter().peekable().map(|tok| *tok);
        let mut toks = Tokenizer::new(first_line).peekable();
        match toks.next().expect("No tokens") {
            "Module" => {
                scope.name = toks.next().unwrap().to_string();
            }
            "ABI" => {}
            "Restrict" => {}
            "Function" => {
                let mut func = parse_func(&mut toks, &mut lines_iter);
                func.restricted = check_restrict(&mut parts);
                scope
                    .properties
                    .insert(func.name.to_owned(), Type::Func(func));
            }
            "Object" => {
                let name = toks.next().expect("Expected Object name");
                assert_eq!(toks.next(), Some("("), "expected arguments");
                let args = parse_func_args(&mut toks);
                let doc = parse_doc(&mut lines_iter);
                let mut obj = Obj {
                    name: name.to_string(),
                    ..Default::default()
                };
                // take all following methods and put into obj
                while let Some(ps) = parts.peek() {
                    let mut lines_iter = ps.split_terminator("\n\n");
                    let first_line = lines_iter.next().unwrap();
                    // let tok_vec = tokenize(first_line, None, None);
                    // let mut toks = tok_vec.iter().peekable().map(|tok| *tok);
                    let mut toks = Tokenizer::new(first_line).peekable();

                    if toks.next() != Some("Method") {
                        break;
                    }

                    parts.next();
                    let mut func = parse_func(&mut toks, &mut lines_iter);
                    func.restricted = check_restrict(&mut parts);
                    obj.properties
                        .insert(func.name.to_owned(), Type::Func(func));
                }
                let func = Func {
                    name: name.to_string(),
                    doc: Some(doc),
                    args,
                    r#return: Some(Box::new(Type::Obj(obj))),
                    ..Default::default()
                };
                scope.properties.insert(name.to_string(), Type::Func(func));
            }
            _ => {}
        }
    }

    Type::Obj(scope)
}

pub fn parse_vcc_file_by_path(
    vcc_path: &std::path::Path,
) -> Result<Type, Box<dyn std::error::Error + Send + Sync>> {
    let vcc_file = std::fs::read_to_string(vcc_path)?;
    let vmod_scope = parse_vcc(vcc_file);
    Ok(vmod_scope)
}

#[cfg(test)]
mod tests {
    use crate::vcc::*;

    #[test]
    fn test_tokenizer() {
        let toks =
            Tokenizer::new("Module directors 3 \"Varnish Directors Module\"").collect::<Vec<_>>();
        assert_eq!(
            toks,
            vec!["Module", "directors", "3", "\"Varnish Directors Module\""]
        );
    }

    #[test]
    fn test_tokenizer_seps() {
        let toks = Tokenizer::new("a=b[c,d]").collect::<Vec<_>>();
        assert_eq!(toks, vec!["a", "=", "b", "[", "c", ",", "d", "]"]);
    }

    #[test]
    fn test_parse_func() {
        let mut toks =
            Tokenizer::new("VOID barf(BOOL rainbow = false, ENUM { IN, OUT } direction = OUT)")
                .peekable();
        let doc = "doc doc doc";
        let func = parse_func(&mut toks, &mut doc.split_terminator("\n"));
        assert_eq!(func.name, "barf");
        assert_eq!(func.args.len(), 2);
        assert_eq!(func.args[0].name, Some("rainbow".into()));
        assert_eq!(func.args[1].name, Some("direction".into()));
        if let Some(Type::Enum(ref enum_vals)) = func.args[1].r#type {
            assert_eq!(*enum_vals, vec!["IN".to_string(), "OUT".to_string()]);
        } else {
            panic!("arg 2 not enum");
        }
        assert_eq!(func.args[1].default_value, Some("OUT".into()));
    }

    #[test]
    fn test_restricted() {
        let Type::Obj(scope) = parse_vcc(
            r#"
$Function VOID my_func()
$Restrict vcl_pipe backend housekeeping
"#
            .to_string(),
        ) else {
            unreachable!()
        };
        let Type::Func(func) = scope.properties.get("my_func").unwrap() else {
            unreachable!()
        };
        assert_eq!(func.name, "my_func");
        assert_eq!(
            func.restricted,
            Some(vec![
                "vcl_pipe".to_string(),
                "backend".to_string(),
                "housekeeping".to_string()
            ])
        );
    }

    #[test]
    fn test_tokenizer_single_token() {
        let toks = Tokenizer::new("License").collect::<Vec<_>>();
        assert_eq!(toks, vec!["License"]);
    }
}
