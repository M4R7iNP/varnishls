/**
 * Inspired by https://www.npmjs.com/package/safe-regex
 */
use regex_syntax::ast::{parse, Ast, RepetitionKind};

#[derive(Debug, PartialEq)]
pub enum SafeRegexError {
    ParseError,
    StarHeightError,
    TooManyRepititions,
}

pub fn is_regex_safe(re_str: String) -> Result<bool, SafeRegexError> {
    let mut parser = parse::Parser::new();
    let Ok(ast) = parser.parse(&re_str) else {
        return Err(SafeRegexError::ParseError);
    };
    let mut total_reps = 0;
    if !walk(&ast, 0, &mut total_reps) {
        return Err(SafeRegexError::StarHeightError);
    }
    if total_reps > 25 {
        return Err(SafeRegexError::TooManyRepititions);
    }
    Ok(true)
}

fn walk(ast: &Ast, star_height: u8, total_reps: &mut u8) -> bool {
    match ast {
        Ast::Concat(concat) => {
            for ast in &concat.asts {
                if !walk(ast, star_height, total_reps) {
                    return false;
                }
            }
            true
        }
        Ast::Repetition(rep) => {
            *total_reps += 1;
            let mut star_height = star_height;
            if rep.greedy
                && matches!(
                    rep.op.kind,
                    RepetitionKind::OneOrMore | RepetitionKind::ZeroOrMore
                )
            {
                star_height += 1;

                if star_height > 1 {
                    return false;
                }
            }
            walk(&rep.ast, star_height, total_reps)
        }
        Ast::Group(group) => walk(&group.ast, star_height, total_reps),
        Ast::Alternation(alt) => {
            for ast in &alt.asts {
                if !walk(ast, star_height, total_reps) {
                    return false;
                }
            }
            true
        }
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid() {
        let re = r"^/nyheter/i/(\w+)".to_string();
        let result = is_regex_safe(re);
        assert_eq!(result, Ok(true));
    }

    #[test]
    fn invalid() {
        let re = "{{{{{{{{[".to_string();
        let result = is_regex_safe(re);
        assert_eq!(result, Err(SafeRegexError::ParseError));
    }

    #[test]
    fn exponential() {
        let re = "(.*)*".to_string();
        let result = is_regex_safe(re);
        assert_eq!(result, Err(SafeRegexError::StarHeightError));
    }

    #[test]
    fn slow() {
        let re = ".*".repeat(26);
        let result = is_regex_safe(re);
        assert_eq!(result, Err(SafeRegexError::TooManyRepititions));
    }
}
