use async_std::sync::{Arc, RwLock};
// use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap, VecDeque};
// use std::result::Result as StdResult;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tree_sitter::Point;

use crate::document::Document;
use crate::varnish_builtins::{get_varnish_builtins, Type};
use crate::vmod::read_vmod_lib_by_name;

pub struct Backend {
    pub client: Client,
    // ast_map: DashMap<String, HashMap<String, Node>>,
    // pub document_map: DashMap<String, Document>,
    pub document_map: Arc<RwLock<HashMap<String, Document>>>,
    // pub root_path: Option<String>,
    pub root_uri: Arc<RwLock<Option<Url>>>,
    // semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,
}

impl Backend {
    pub fn new(client: Client) -> Backend {
        Backend {
            client,
            document_map: Default::default(),
            root_uri: Default::default(),
        }
    }

    async fn get_all_definitions_across_all_documents(&self) -> Type {
        let mut scope = get_varnish_builtins();
        let doc_map = self.document_map.read().await;
        if let Type::Obj(ref mut obj) = scope {
            for (_key, doc) in doc_map.iter() {
                for vmod_name in doc.get_vmod_imports() {
                    let vmod = read_vmod_lib_by_name(vmod_name.clone()).await;
                    if let Ok(vmod) = vmod {
                        let vmod_obj = vmod.scope;
                        obj.properties.insert(vmod_name, vmod_obj);
                    }
                }
            }

            for (_key, doc) in doc_map.iter() {
                for def in doc.get_all_definitions() {
                    obj.properties
                        .insert(def.ident_str.to_string(), *def.r#type);
                }
            }
        }

        let mut temp_map = BTreeMap::new();

        for (_key, doc) in doc_map.iter() {
            for def in doc.get_new_objs(&scope) {
                temp_map.insert(def.ident_str.to_string(), *def.r#type);
            }
        }
        if let Type::Obj(ref mut obj) = scope {
            obj.properties.append(&mut temp_map);
        }

        return scope;
    }

    async fn set_root_uri(&self, uri: Url) {
        let mut w = self.root_uri.write().await;
        *w = Some(uri);
    }

    async fn read_includes(
        &self,
        doc_uri: String,
        includes: Vec<String>,
    ) -> HashMap<String, Document> {
        let mut new_docs = HashMap::new();
        let mut includes_to_process = VecDeque::from(includes.clone());

        self.client
            .log_message(MessageType::INFO, format!("includes: {:?}", includes))
            .await;

        let uri = Url::parse(&doc_uri).unwrap();
        let doc_path = uri.to_file_path().unwrap().parent().unwrap().to_owned();
        loop {
            let include = match includes_to_process.pop_front() {
                Some(val) => val,
                None => break,
            };

            let include_path_str = doc_path.join(include).to_string_lossy().to_string();
            if new_docs.contains_key(&include_path_str) {
                continue;
            }

            let file = tokio::fs::read_to_string(&include_path_str).await;
            if let Err(err) = file {
                self.client
                    .publish_diagnostics(
                        uri.clone(),
                        vec![Diagnostic {
                            severity: Some(DiagnosticSeverity::ERROR),
                            message: format!("Failed to read file {} ({})", include_path_str, err),
                            ..Default::default()
                        }],
                        None,
                    )
                    .await;
                continue;
            }

            let file = file.unwrap();
            let mut included_doc = Document::new(file);
            included_doc.url = Some(include_path_str.clone());
            includes_to_process.append(&mut included_doc.get_includes().into());
            new_docs.insert(include_path_str.clone(), included_doc);
        }

        return new_docs;
    }
}

unsafe impl Send for Backend {}
unsafe impl Sync for Backend {}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, init_params: InitializeParams) -> Result<InitializeResult> {
        self.set_root_uri(init_params.root_uri.clone().unwrap())
            .await;

        /*
        self.client
            .log_message(MessageType::INFO, format!("initializing: {:?}", init_params))
            .await;
        */

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "varnish_lsp".to_string(),
                version: None,
            }),
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                    // TextDocumentSyncKind::FULL,
                )),

                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![".".to_string(), " ".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: None,
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let mut document = Document::new(params.text_document.text);
        document.url = Some(uri.to_string());
        let mut doc_map = self.document_map.write().await;
        doc_map.insert(uri.to_string(), document);

        let doc = doc_map.get(&uri.to_string()).unwrap();
        self.client
            .publish_diagnostics(
                params.text_document.uri,
                doc.diagnostics(),
                Some(doc.version()),
            )
            .await;

        let includes = doc.get_includes();
        // drop(doc_map); // drop reference, unlocks lock
        let included_docs = self.read_includes(uri, includes).await;
        for (url_str, included_doc) in included_docs {
            doc_map.insert(url_str, included_doc);
        }
        self.client.log_message(MessageType::INFO, "All included files are imported").await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let version = params.text_document.version;
        let changes = params
            .content_changes
            .into_iter()
            .map(|change| (change.range, change.text));

        let mut doc_map = self.document_map.write().await;
        let doc = doc_map.get_mut(&uri).unwrap();
        doc.edit(version, changes);

        // let doc = self.document_map.get(&uri.clone()).unwrap();
        self.client
            .publish_diagnostics(
                params.text_document.uri,
                doc.diagnostics(),
                Some(doc.version()),
            )
            .await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let src_uri = params.text_document_position_params.text_document.uri;
        let doc_map = self.document_map.read().await;
        let src_doc = doc_map.get(&src_uri.to_string()).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find source document".to_string(),
            data: None,
        })?;
        let position = params.text_document_position_params.position;
        let point = Point {
            row: position.line as usize,
            column: position.character as usize,
        };

        let ident = src_doc.get_ident_at_point(point).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find ident".to_string(),
            data: None,
        })?;

        for (uri, doc) in doc_map.iter() {
            let result = doc.get_definition_by_name(ident.clone());
            if result.is_none() {
                continue;
            }

            let (start, end) = result.unwrap();

            let range = Range::new(
                Position {
                    line: start.row as u32,
                    character: start.column as u32,
                },
                Position {
                    line: end.row as u32,
                    character: end.column as u32,
                },
            );

            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                Url::parse(uri).unwrap(),
                range,
            ))));
        }

        return Ok(None);
    }

    /*
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = || -> Option<Vec<Location>> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let reference_list = get_reference(&ast, offset, false);
            let ret = reference_list
                .into_iter()
                .filter_map(|(_, range)| {
                    let start_position = offset_to_position(range.start, &rope)?;
                    let end_position = offset_to_position(range.end, &rope)?;

                    let range = Range::new(start_position, end_position);

                    Some(Location::new(uri.clone(), range))
                })
                .collect::<Vec<_>>();
            Some(ret)
        }();
        Ok(reference_list)
    }
    */

    /*
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let ast = self.ast_map.get(&uri)?;
            let extends_tokens = semantic_token_from_ast(&ast);
            im_complete_tokens.extend(extends_tokens);
            im_complete_tokens.sort_by(|a, b| a.start.cmp(&b.start));
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
                    let delta_line = line - pre_line;
                    let delta_start = if delta_line == 0 {
                        start - pre_start
                    } else {
                        start
                    };
                    let ret = Some(SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let im_complete_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
                    let ret = Some(SemanticToken {
                        delta_line: line - pre_line,
                        delta_start: if start >= pre_start {
                            start - pre_start
                        } else {
                            start
                        },
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }
    */

    /*
    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        self.client
            .log_message(MessageType::INFO, "inlay hint")
            .await;
        let uri = &params.text_document.uri;
        let mut hashmap = HashMap::new();
        if let Some(ast) = self.ast_map.get(uri.as_str()) {
            ast.iter().for_each(|(_, v)| {
                type_inference(&v.body, &mut hashmap);
            });
        }

        let document = match self.document_map.get(uri.as_str()) {
            Some(rope) => rope,
            None => return Ok(None),
        };
        let inlay_hint_list = hashmap
            .into_iter()
            .map(|(k, v)| {
                (
                    k.start,
                    k.end,
                    match v {
                        nrs_language_server::chumsky::Value::Null => "null".to_string(),
                        nrs_language_server::chumsky::Value::Bool(_) => "bool".to_string(),
                        nrs_language_server::chumsky::Value::Num(_) => "number".to_string(),
                        nrs_language_server::chumsky::Value::Str(_) => "string".to_string(),
                        nrs_language_server::chumsky::Value::List(_) => "[]".to_string(),
                        nrs_language_server::chumsky::Value::Func(_) => v.to_string(),
                    },
                )
            })
            .filter_map(|item| {
                // let start_position = offset_to_position(item.0, document)?;
                let end_position = offset_to_position(item.1, &document)?;
                let inlay_hint = InlayHint {
                    text_edits: None,
                    tooltip: None,
                    kind: Some(InlayHintKind::TYPE),
                    padding_left: None,
                    padding_right: None,
                    data: None,
                    position: end_position,
                    label: InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
                        value: item.2,
                        tooltip: None,
                        location: Some(Location {
                            uri: params.text_document.uri.clone(),
                            range: Range {
                                start: Position::new(0, 4),
                                end: Position::new(0, 5),
                            },
                        }),
                        command: None,
                    }]),
                };
                Some(inlay_hint)
            })
            .collect::<Vec<_>>();

        Ok(Some(inlay_hint_list))
    }
    */

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let scope = self.get_all_definitions_across_all_documents().await;
        // let scope = get_varnish_builtins();
        let doc_map = self.document_map.read().await;
        let completions = || -> Option<Vec<CompletionItem>> {
            let doc = doc_map.get(&uri.to_string())?;

            return doc.autocomplete_for_pos(position, scope);
        }();
        Ok(completions.map(CompletionResponse::Array))
    }

    /*
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = || -> Option<WorkspaceEdit> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let reference_list = get_reference(&ast, offset, true);
            let new_name = params.new_name;
            if reference_list.len() > 0 {
                let edit_list = reference_list
                    .into_iter()
                    .filter_map(|(_, range)| {
                        let start_position = offset_to_position(range.start, &rope)?;
                        let end_position = offset_to_position(range.end, &rope)?;
                        Some(TextEdit::new(
                            Range::new(start_position, end_position),
                            new_name.clone(),
                        ))
                    })
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(uri, edit_list);
                let workspace_edit = WorkspaceEdit::new(map);
                Some(workspace_edit)
            } else {
                None
            }
        }();
        Ok(workspace_edit)
    }
    */

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
