// use log::debug;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::path::{Path, PathBuf};
use tokio::sync::RwLock;
use toml;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tree_sitter::Point;

use crate::config::Config;
use crate::document::Document;
use crate::varnish_builtins::{get_varnish_builtins, Type};
use crate::vmod::read_vmod_lib_by_name;

pub struct Backend {
    pub client: Client,
    // ast_map: DashMap<String, HashMap<String, Node>>,
    // pub document_map: DashMap<String, Document>,
    pub document_map: RwLock<HashMap<String, Document>>,
    // pub root_path: Option<String>,
    pub root_uri: RwLock<Option<Url>>,
    pub config: RwLock<Config>,
    // semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,
}

impl Backend {
    pub fn new(client: Client) -> Backend {
        Backend {
            client,
            document_map: Default::default(),
            root_uri: Default::default(),
            config: Default::default(),
        }
    }

    async fn get_all_definitions_across_all_documents(&self) -> Type {
        let config = self.config.read().await;
        let mut scope = get_varnish_builtins();
        let doc_map = self.document_map.read().await;
        if let Type::Obj(ref mut obj) = scope {
            // read all vmods
            let vmod_futures = doc_map
                .values()
                .flat_map(|doc| doc.get_vmod_imports())
                .map(|ref vmod_name| {
                    tokio::task::spawn(read_vmod_lib_by_name(
                        vmod_name.into(),
                        config.vmod_paths.to_owned(),
                    ))
                })
                .collect::<Vec<_>>();

            for vmod_fut in vmod_futures {
                if let Ok(Ok(Some(vmod))) = vmod_fut.await {
                    let vmod_obj = vmod.scope;
                    obj.properties.insert(vmod.name, vmod_obj);
                }
            }
        }

        // all objs (e.g. «new awdawd = new director.round_robin()»)
        let mut temp_map = BTreeMap::from_iter(
            doc_map
                .values()
                .flat_map(|doc| doc.get_all_definitions(&scope))
                .map(|def| (def.ident_str.to_string(), *def.r#type)),
        );

        if let Type::Obj(ref mut obj) = scope {
            obj.properties.append(&mut temp_map);
        }

        scope
    }

    async fn set_root_uri(&self, uri: Url) {
        let mut w = self.root_uri.write().await;
        *w = Some(uri);
    }

    async fn set_config(&self, config: Config) {
        let mut w = self.config.write().await;
        *w = config;
    }

    /*
     * TODO: doc_uri should be «main vcl» uri unless the import starts with «./»
     * TODO: parallelize with tokio?
     */
    async fn read_includes(
        &self,
        doc_uri: Url,
        includes: VecDeque<String>,
    ) -> HashMap<String, Document> {
        let mut new_docs = HashMap::new();
        let mut includes_to_process = includes.clone();

        while let Some(include) = includes_to_process.pop_front() {
            let include_uri = match doc_uri.join(&include) {
                Ok(uri) => uri,
                Err(_err) => continue,
            };
            let include_file_uri = include_uri.to_file_path().unwrap();
            let include_file_path_str = include_file_uri.to_string_lossy().to_string();
            let doc_already_exists = {
                let map = self.document_map.read().await;
                map.contains_key(&include_file_path_str)
            };
            if doc_already_exists || new_docs.contains_key(&include_file_path_str) {
                continue;
            }

            let file = tokio::fs::read_to_string(&include_file_path_str).await;
            if let Err(err) = file {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to read file {} ({})", include_file_path_str, err),
                    )
                    .await;
                continue;
            }

            let file = file.unwrap();
            let included_doc = Document::new(include_uri, file);
            includes_to_process.append(&mut included_doc.get_includes());
            new_docs.insert(include_file_path_str.clone(), included_doc);
        }

        new_docs
    }

    async fn read_doc_from_path(&self, doc_url: &Url) {
        let file = tokio::fs::read_to_string(&doc_url.to_file_path().unwrap()).await;
        if let Err(err) = file {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Failed to read file {} ({})", doc_url, err),
                )
                .await;

            return;
        }

        let file = file.unwrap();
        let doc = Document::new(doc_url.clone(), file);
        let mut doc_map = self.document_map.write().await;
        doc_map.insert(doc.url.to_string(), doc);
    }
}

unsafe impl Send for Backend {}
unsafe impl Sync for Backend {}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, init_params: InitializeParams) -> Result<InitializeResult> {
        let root_uri = init_params.root_uri;
        // TODO: consider not initializing if uri scheme is not file

        if let Some(root_uri) = root_uri {
            self.set_root_uri(root_uri.clone()).await;

            if root_uri.scheme() == "file" {
                let mut root_uri = root_uri;
                /*
                 * Fix workspace directory missing slash
                 */
                if !root_uri.path().ends_with('/') {
                    let root_path = root_uri.to_file_path().unwrap();
                    if root_path.as_path().is_dir() {
                        root_uri.set_path(format!("{}/", root_uri.path()).as_str());
                        self.set_root_uri(root_uri.clone()).await;
                    }
                }

                let config = read_config(&root_uri.to_file_path().unwrap()).await;
                if let Some(config) = config {
                    self.set_config(config).await;
                }

                let config = self.config.read().await;
                if let Some(ref main_vcl_path) = config.main_vcl {
                    let main_vcl_url = root_uri.join(main_vcl_path.to_str().unwrap()).unwrap();
                    self.read_doc_from_path(&main_vcl_url).await;
                    let doc_map = self.document_map.read().await;
                    let main_doc = doc_map.get(&main_vcl_url.to_string()).unwrap();
                    let includes = main_doc.get_includes();
                    drop(doc_map); // drop reference to unlock
                    let included_docs = self.read_includes(main_vcl_url, includes).await;
                    let mut doc_map = self.document_map.write().await;
                    for (url_str, included_doc) in included_docs {
                        doc_map.insert(url_str, included_doc);
                    }
                    drop(doc_map); // drop reference to unlock
                }
            }
        }

        /*
        self.client
            .log_message(MessageType::INFO, format!("initializing: {:?}", init_params))
            .await;
        */

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "varnishls".to_string(),
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
                    resolve_provider: Some(false),
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
        let uri = params.text_document.uri;
        let uri_str = uri.to_string();
        let document = Document::new(uri.to_owned(), params.text_document.text);
        let mut doc_map = self.document_map.write().await;
        doc_map.insert(uri_str, document);

        let doc = doc_map.get(&uri.to_string()).unwrap();
        self.client
            .publish_diagnostics(uri.clone(), doc.diagnostics(), Some(doc.version()))
            .await;

        let includes = doc.get_includes();
        drop(doc_map); // drop reference, unlocks lock
        let included_docs = self.read_includes(uri, includes).await;
        for (url_str, included_doc) in included_docs {
            let mut doc_map = self.document_map.write().await;
            doc_map.insert(url_str, included_doc);
        }
        self.client
            .log_message(MessageType::INFO, "All included files are imported")
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let changes = params
            .content_changes
            .into_iter()
            .map(|change| (change.range, change.text));

        let mut doc_map = self.document_map.write().await;
        let doc = doc_map.get_mut(&uri.to_string()).unwrap();
        doc.edit(version, changes);

        // let doc = self.document_map.get(&uri.clone()).unwrap();
        self.client
            .publish_diagnostics(uri.clone(), doc.diagnostics(), Some(doc.version()))
            .await;

        let includes = doc.get_includes();
        drop(doc_map);
        let included_docs = self.read_includes(uri, includes).await;
        for (url_str, included_doc) in included_docs {
            let mut doc_map = self.document_map.write().await;
            doc_map.insert(url_str, included_doc);
        }
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

        for (_uri, doc) in doc_map.iter() {
            let result = doc.get_definition_by_name(&ident);
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
                doc.url.to_owned(),
                range,
            ))));
        }

        return Ok(None);
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let src_uri = params.text_document_position.text_document.uri;
        let doc_map = self.document_map.read().await;
        let doc = doc_map.get(&src_uri.to_string()).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find source document".to_string(),
            data: None,
        })?;
        let position = params.text_document_position.position;
        let point = Point {
            row: position.line as usize,
            column: position.character as usize,
        };

        let ident = doc.get_ident_at_point(point).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find ident".to_string(),
            data: None,
        })?;

        let refs = doc_map
            .values()
            .flat_map(|doc| doc.get_references_for_ident(ident.as_str()))
            .map(|reference| reference.uri)
            .collect::<Vec<_>>();

        if refs.is_empty() {
            return Ok(None);
        }

        Ok(Some(refs))
    }

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

            doc.autocomplete_for_pos(position, scope)
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

async fn read_config(root_path: &Path) -> Option<Config> {
    let config_path_buf = root_path.join(PathBuf::from(".varnishls.toml"));
    let config_path = config_path_buf.as_path();
    if !config_path.exists() {
        return None;
    }
    let config_str = tokio::fs::read_to_string(config_path).await;
    if let Ok(config_str) = config_str {
        let config = toml::from_str(&config_str);
        return Some(config.unwrap());
    }
    None
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
