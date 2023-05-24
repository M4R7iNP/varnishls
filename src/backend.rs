use glob::{glob, GlobResult};
use log::{debug, error};
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
use crate::document::{Document, Include, NestedPos};
use crate::varnish_builtins::{get_varnish_builtins, Definition, Definitions, Type};
use crate::vcc::parse_vcc_file_by_path;
use crate::vmod::read_vmod_lib_by_name;

pub struct Backend {
    pub client: Client,
    // ast_map: DashMap<String, HashMap<String, Node>>,
    // pub document_map: DashMap<String, Document>,
    pub document_map: RwLock<HashMap<Url, Document>>,
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

    /**
     * Gathers all defined identifiers across all documents loaded, and
     * then parses vmods either by vmod lib binary or vcc file
     *
     * parse_vcc and parse_vmod both contains runtime assertions, so they
     * are ran in their own threads.
     */
    async fn get_all_definitions_across_all_documents(&self, src_doc_url: &Url) -> Definitions {
        debug!("get_all_definitions_across_all_documents()");
        let start = std::time::Instant::now();
        let config = self.config.read().await;
        let doc_map = self.document_map.read().await;
        let root_uri = self.root_uri.read().await;
        let mut definitions = get_varnish_builtins();

        let documents_from_main_in_order = {
            if let Some(ref main_vcl_path) = config.main_vcl {
                // make root url the main vcl url without the filename
                let root_uri = root_uri
                    .as_ref()
                    .unwrap()
                    .join(&main_vcl_path.to_string_lossy())
                    .unwrap();
                // main_vcl_path is now just the filename
                let main_vcl_path = main_vcl_path
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                get_all_documents(&doc_map, &root_uri, &main_vcl_path, &vec![])
            } else {
                doc_map
                    .get(src_doc_url)
                    .map(|doc| vec![doc])
                    .unwrap_or(vec![])
            }
        };

        let all_vmod_imports = documents_from_main_in_order
            .iter()
            .flat_map(|doc| doc.get_vmod_imports())
            .fold(HashMap::new(), |mut map, import| {
                if !map.contains_key(&import.name) {
                    map.insert(import.name.clone(), import);
                }
                map
            });

        /*
         * read all vcc files by glob (expects e.g. "{vcc_files_dir}/libvmod_std/vmod.vcc")
         *
         * TODO: maybe read all vcc files at startup, cache them, and then lookup from here.
         * TODO: support pointing to singular vcc file or vmod src dir containing a vcc file
         */
        let vcc_files: Vec<_> = all_vmod_imports
            .values()
            .flat_map(|import| {
                config
                    .vcc_paths
                    .iter()
                    .flat_map(|vcc_path| -> Vec<GlobResult> {
                        // glob(format!("{}/libvmod_{}/*.vcc", vcc_path, vmod_name))
                        let mut glob_path = vcc_path.clone();
                        glob_path.push(format!("libvmod_{}", import.name));
                        glob_path.push("*.vcc");
                        glob(glob_path.to_str().unwrap())
                            .map(|paths| paths.collect::<Vec<GlobResult>>())
                            .unwrap_or_else(|_| vec![])
                    })
                    .collect::<Vec<_>>()
            })
            .filter_map(|result| result.ok())
            .collect::<Vec<_>>();

        // parse each vcc file in their own thread.
        let mut set = tokio::task::JoinSet::new();
        for vcc_file_path in vcc_files {
            set.spawn(async move { parse_vcc_file_by_path(&vcc_file_path) });
        }

        while let Some(result) = set.join_next().await {
            let result = result
                .map_err(Box::<dyn std::error::Error + Send + Sync>::from) // map JoinError into generic error
                .and_then(|result| result); // replace with result from parse_vcc_file_by_path

            match result {
                Err(err) => {
                    self.emit_error(err.to_string()).await;
                }
                Ok(vmod_scope) => {
                    let vmod_name = match vmod_scope {
                        Type::Obj(ref obj) => obj.name.clone(),
                        _ => unreachable!(),
                    };
                    let import = all_vmod_imports.get(&vmod_name);
                    let def = Definition {
                        ident_str: vmod_name.clone(),
                        r#type: Box::new(vmod_scope),
                        loc: import.map(|import| import.loc.clone()),
                        nested_pos: import.and_then(|import| import.nested_pos.clone()),
                    };
                    definitions.properties.insert(vmod_name, def);
                }
            }
        }

        // read all vmods
        let vmod_futures = all_vmod_imports
            .keys()
            .filter(|vmod_name| !definitions.properties.contains_key(*vmod_name)) // filter out vmods found by vcc
            .map(|ref vmod_name| {
                // debug!("spawning task to vmod binary for «{vmod_name}»");
                tokio::task::spawn(read_vmod_lib_by_name(
                    vmod_name.to_string(),
                    config.vmod_paths.to_owned(),
                ))
            })
            .collect::<Vec<_>>();

        for vmod_fut in vmod_futures {
            if let Ok(Ok(Some(vmod))) = vmod_fut.await {
                let vmod_name = vmod.name;
                let import = all_vmod_imports.get(&vmod_name);
                let def = Definition {
                    ident_str: vmod_name.clone(),
                    r#type: Box::new(vmod.scope),
                    loc: import.map(|import| import.loc.clone()),
                    nested_pos: import.and_then(|import| import.nested_pos.clone()),
                };
                definitions.properties.insert(vmod_name, def);
            }
        }

        // all objs (e.g. «new awdawd = new director.round_robin()»)
        let mut temp_map = BTreeMap::from_iter(
            documents_from_main_in_order
                .iter()
                .flat_map(|doc| doc.get_all_definitions(&definitions))
                .map(|def| (def.ident_str.to_string(), def)),
        );

        definitions.properties.append(&mut temp_map);
        debug!(
            "get_all_definitions_across_all_documents() done in {}ms",
            start.elapsed().as_millis()
        );
        definitions
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
    async fn read_new_includes(&self, doc_uri: Url, includes: Vec<Include>) {
        debug!("read_new_includes()");
        let mut includes_to_process = VecDeque::from(includes);

        while let Some(include) = includes_to_process.pop_front() {
            let Ok(include_uri) = doc_uri.join(&include.path_str) else {
                continue;
            };
            let include_file_path = include_uri.to_file_path().unwrap();
            let include_file_path_str = include_file_path.to_string_lossy().to_string();
            let doc_already_exists = {
                let map = self.document_map.read().await;
                map.contains_key(&include_uri)
            };
            if doc_already_exists {
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
            let included_doc =
                Document::new(include_uri.clone(), file, Some(include.nested_pos.clone()));
            let mut doc_map = self.document_map.write().await;
            includes_to_process.append(&mut included_doc.get_includes(&include.nested_pos).into());
            doc_map.insert(include_uri.clone(), included_doc);
        }
    }

    async fn read_doc_from_path(&self, doc_url: &Url, nested_pos: Option<NestedPos>) {
        let file_path = doc_url.to_file_path().unwrap();
        let file = tokio::fs::read_to_string(&file_path).await;
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
        let doc = Document::new(doc_url.clone(), file, nested_pos);
        let mut doc_map = self.document_map.write().await;
        doc_map.insert(doc_url.clone(), doc);
    }

    async fn emit_error(&self, err_msg: String) {
        error!("{err_msg}");
        self.client.log_message(MessageType::ERROR, err_msg).await;
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
                    self.read_doc_from_path(&main_vcl_url, Some(vec![])).await;
                    let includes = {
                        let doc_map = self.document_map.read().await;
                        let main_doc = doc_map.get(&main_vcl_url).unwrap();
                        main_doc.get_includes(&vec![])
                    };
                    self.read_new_includes(main_vcl_url, includes).await;
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
                inlay_hint_provider: Some(OneOf::Left(false)),
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
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
        debug!("did_open()");
        let uri = params.text_document.uri;
        let exists = {
            let doc_map = self.document_map.read().await;
            doc_map.contains_key(&uri)
        };
        if !exists {
            let document = Document::new(uri.to_owned(), params.text_document.text, None);

            let mut doc_map = self.document_map.write().await;
            doc_map.insert(uri.clone(), document);
            drop(doc_map);
        }

        let scope = self.get_all_definitions_across_all_documents(&uri).await;
        let doc_map = self.document_map.read().await;
        let doc = doc_map.get(&uri).unwrap();
        self.client
            .publish_diagnostics(uri.clone(), doc.diagnostics(scope), Some(doc.version()))
            .await;

        let includes = doc.get_includes(doc.pos_from_main_doc.as_ref().unwrap_or(&vec![]));
        drop(doc_map); // drop reference, unlocks lock
        self.read_new_includes(uri, includes).await;
        self.client
            .log_message(MessageType::INFO, "All included files are imported")
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        debug!("did_change()");
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let changes = params
            .content_changes
            .into_iter()
            .map(|change| (change.range, change.text));

        {
            let mut doc_map = self.document_map.write().await;
            let doc = doc_map.get_mut(&uri).unwrap();
            doc.edit(version, changes);
        }

        let scope = self.get_all_definitions_across_all_documents(&uri).await;
        let doc_map = self.document_map.read().await;
        let doc = doc_map.get(&uri).unwrap();

        self.client
            .publish_diagnostics(uri.clone(), doc.diagnostics(scope), Some(doc.version()))
            .await;

        let includes = doc.get_includes(doc.pos_from_main_doc.as_ref().unwrap_or(&vec![]));
        drop(doc_map); // drop reference, unlocks lock
        self.read_new_includes(uri, includes).await;
        debug!("did_change() done!");
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
        let src_doc = doc_map.get(&src_uri).ok_or(Error {
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

        debug!("goto definition for ident «{}»", ident);

        for doc in doc_map.values() {
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

        debug!("could not find definition");
        return Ok(None);
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let src_uri = params.text_document_position.text_document.uri;
        let doc_map = self.document_map.read().await;
        let doc = doc_map.get(&src_uri).ok_or(Error {
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
        let scope = self.get_all_definitions_across_all_documents(&uri).await;
        let doc_map = self.document_map.read().await;
        let doc = doc_map.get(&uri).unwrap();
        let completions = doc.autocomplete_for_pos(position, scope);
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let scope = self.get_all_definitions_across_all_documents(&uri).await;
        let doc_map = self.document_map.read().await;
        let doc = doc_map.get(&uri).unwrap();
        let pos = params.text_document_position_params.position;
        let point = Point {
            row: pos.line as usize,
            column: pos.character as usize,
        };
        let r#type = match doc.get_type_at_point(point, scope) {
            Some(r#type) => r#type,
            None => return Ok(None),
        };

        if let Type::Func(func) = r#type {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        "{} {}{}{}",
                        func.r#return
                            .as_ref()
                            .map(|r| format!("{}", r))
                            .unwrap_or_else(|| "VOID".to_string()),
                        func.name,
                        func.get_signature_string(),
                        func.doc
                            .map(|doc| format!("\n\n{doc}"))
                            .unwrap_or_else(|| "".to_string())
                    ),
                }),
                range: None,
            }));
        }

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::PlainText,
                value: format!("{}", r#type),
            }),
            range: None,
        }))
    }

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

fn get_all_documents<'a>(
    doc_map: &'a HashMap<Url, Document>,
    root_uri: &Url,
    doc_path: &str,
    nested_pos: &NestedPos,
) -> Vec<&'a Document> {
    let doc_url = root_uri.join(doc_path).unwrap();
    let Some(main_doc) = doc_map.get(&doc_url) else {
        debug!("Could not find document {}", doc_url);
        return vec![];
    };

    main_doc
        .get_includes(nested_pos)
        .iter()
        .flat_map(|include| {
            get_all_documents(doc_map, root_uri, &include.path_str, &include.nested_pos)
        })
        .collect()
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
