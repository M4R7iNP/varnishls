use tower_lsp::lsp_types::*;

fn get_if_completion() -> CompletionItem {
    CompletionItem {
        label: "if".to_string(),
        detail: Some("if".to_string()),
        kind: Some(CompletionItemKind::KEYWORD),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        insert_text_mode: Some(InsertTextMode::ADJUST_INDENTATION),
        insert_text: Some("if (${1}) {\n${2}\n}".to_string()),
        ..Default::default()
    }
}

pub fn source_file() -> Vec<CompletionItem> {
    let mut statements = vec![get_if_completion()];

    ["sub", "acl", "backend", "probe"]
        .iter()
        .map(|field| CompletionItem {
            label: field.to_string(),
            detail: Some(field.to_string()),
            kind: Some(CompletionItemKind::KEYWORD),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            insert_text: Some(format!("{} ${{1}} {{\n${{2}}\n}}", field)),
            ..Default::default()
        })
        .for_each(|item| statements.push(item));

    ["include", "import"]
        .iter()
        .map(|keyword| CompletionItem {
            label: keyword.to_string(),
            detail: Some(keyword.to_string()),
            kind: Some(CompletionItemKind::KEYWORD),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            insert_text: Some(format!("{} \"${{1}}\";", keyword)),
            ..Default::default()
        })
        .for_each(|item| statements.push(item));

    return statements;
}

pub fn subroutine() -> Vec<CompletionItem> {
    vec![
        get_if_completion(),
        CompletionItem {
            label: "include".to_string(),
            detail: Some("include".to_string()),
            kind: Some(CompletionItemKind::KEYWORD),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            insert_text: Some("include \"${1}\";".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "set".to_string(),
            detail: Some("set".to_string()),
            kind: Some(CompletionItemKind::KEYWORD),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            insert_text: Some("set ${1} = ${2};".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "new".to_string(),
            detail: Some("new".to_string()),
            kind: Some(CompletionItemKind::KEYWORD),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            insert_text: Some("new ${1} = ${2};".to_string()),
            ..Default::default()
        },
    ]
}
