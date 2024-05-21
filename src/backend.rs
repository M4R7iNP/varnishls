#![allow(deprecated)]
use dashmap::DashMap;
use log::{debug, error};
use std::collections::{BTreeMap, VecDeque};
use std::path::{Path, PathBuf};
use tokio::sync::RwLock;
use toml;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tree_sitter::Point;

use crate::config::Config;
use crate::document::{Document, Include, NestedPos, VmodImport, LEGEND_TYPES};
use crate::varnish_builtins::{get_varnish_builtins, Definition, Definitions, Type};
use crate::vcc::parse_vcc_file_by_path;
use crate::vmod::read_vmod_lib_by_name;

#[derive(Debug, Default)]
pub struct CacheEntry {
    includes: Option<Vec<Include>>,
    vmod_imports: Option<Vec<VmodImport>>,
    definitions: Option<Vec<Definition>>,
}

type DocumentMap = DashMap<Url, Document>;

pub struct Backend {
    pub client: Option<Client>,
    pub document_map: DocumentMap,
    pub root_uri: RwLock<Url>,
    pub root_document_uri: RwLock<Option<Url>>,
    pub config: RwLock<Config>,
    /// cache to cache e.g. stat-ing includes and discovering definitions in all documents
    pub cache: DashMap<Url, CacheEntry>,
}

impl Backend {
    pub fn new(client: Client) -> Backend {
        Backend {
            client: Some(client),
            document_map: Default::default(),
            root_uri: RwLock::new(
                Url::from_directory_path(std::env::current_dir().unwrap()).unwrap(),
            ),
            root_document_uri: Default::default(),
            config: Default::default(),
            cache: Default::default(),
        }
    }

    /**
     * Gathers all defined identifiers across all documents loaded, and
     * then parses vmods either by vmod lib binary or vcc file
     *
     * parse_vcc and parse_vmod both contains runtime assertions, so they
     * are ran in their own threads.
     */
    pub async fn get_all_definitions_across_all_documents(
        &self,
        src_doc_url: Option<&Url>,
    ) -> Definitions {
        debug!("get_all_definitions_across_all_documents()");
        let start = std::time::Instant::now();
        let config = self.config.read().await;
        let mut definitions = get_varnish_builtins();

        let documents_from_main_in_order = {
            let mut docs = vec![];

            if let Some(ref root_document_uri) = *self.root_document_uri.read().await {
                docs =
                    get_all_documents(&self.document_map, &self.cache, &config, root_document_uri);
            } else if let Some(src_doc_url) = src_doc_url {
                docs = get_all_documents(&self.document_map, &self.cache, &config, src_doc_url);
            }

            // if this doc is not included from main vcl, just append it
            if let Some(src_doc_url) = src_doc_url {
                if !docs.iter().any(|doc_url| doc_url == src_doc_url)
                    && self.document_map.contains_key(src_doc_url)
                {
                    docs.push(src_doc_url.clone());
                }
            }

            // debug!("got {} docs", docs.len());

            docs
        };

        // gather all vmod imports, but only keep the first of each unique vmod
        let all_vmod_imports: Vec<VmodImport> = documents_from_main_in_order
            .iter()
            .flat_map(|doc_url| -> Vec<VmodImport> {
                let mut cache_entry = self.cache.entry((*doc_url).clone()).or_default();
                cache_entry
                    .vmod_imports
                    .get_or_insert_with(|| {
                        self.document_map
                            .get(doc_url)
                            .map(|doc| doc.get_vmod_imports())
                            .unwrap_or_default()
                    })
                    .clone()
            })
            .fold(vec![], |mut set, import| {
                if !set.contains(&import) {
                    set.push(import);
                }
                set
            });

        // read all vmods
        let mut vmod_scope = read_all_vmods(all_vmod_imports, &config).await;
        definitions.properties.append(&mut vmod_scope.properties);

        // all objs (e.g. «new awdawd = new director.round_robin()»)
        let mut temp_map: BTreeMap<String, Definition> = BTreeMap::from_iter(
            documents_from_main_in_order
                .iter()
                .flat_map(|doc_url| {
                    let mut cache_entry = self.cache.entry((*doc_url).clone()).or_default();
                    cache_entry
                        .definitions
                        .get_or_insert_with(|| {
                            self.document_map
                                .get(doc_url)
                                .map(|doc| doc.get_all_definitions(&definitions))
                                .unwrap_or_default()
                        })
                        .clone()
                })
                .map(|def| (def.ident_str.to_string(), def)),
        );

        definitions.properties.append(&mut temp_map);
        debug!(
            "get_all_definitions_across_all_documents() done in {}ms",
            start.elapsed().as_millis()
        );
        definitions
    }

    pub async fn set_root_uri(&self, uri: Url) {
        let mut w = self.root_uri.write().await;
        *w = uri;
    }

    pub async fn set_config(&self, config: Config) {
        let mut w = self.config.write().await;
        *w = config;
    }

    /*
     * TODO: doc_uri should be «main vcl» uri unless the import starts with «./»
     * TODO: parallelize with tokio?
     */
    pub async fn read_new_includes(&self, initial_includes: Vec<Include>) {
        debug!("read_new_includes()");
        let config = self.config.read().await;
        let mut includes_to_process = VecDeque::from(initial_includes);

        while let Some(include) = includes_to_process.pop_front() {
            let mut include = include;
            if include.url.is_none() {
                include = include.resolve(&config.vcl_paths);
            }

            let Some(include_url) = include.url else {
                debug!("Could not find include {:?}", include.path);
                continue;
            };

            let doc_already_exists = {
                if let Some(doc) = self.document_map.get(&include_url) {
                    // Update nested_pos if it doesn't match
                    if doc.pos_from_main_doc != include.nested_pos {
                        drop(doc);
                        let mut doc = self.document_map.get_mut(&include_url).unwrap();
                        doc.pos_from_main_doc = include.nested_pos.clone();
                        // ... do the same for nested includes
                        includes_to_process.append(&mut doc.get_includes().into());
                        drop(doc);
                    }
                    true
                } else {
                    false
                }
            };
            if doc_already_exists {
                continue;
            }

            let Some(include_uri) = self
                .read_doc_from_path(include.path.as_path(), include.nested_pos)
                .await
            else {
                // error should already have been logged
                continue;
            };

            let Some(included_doc) = self.document_map.get(&include_uri) else {
                continue;
            };

            // Wipe cache for this nested doc and read includes.
            let mut cache_entry = CacheEntry::default();
            let nested_includes = included_doc
                .get_includes()
                .into_iter()
                .map(|include| include.resolve(&config.vcl_paths))
                .collect::<Vec<_>>();
            cache_entry.includes = Some(nested_includes.clone());
            includes_to_process.append(&mut nested_includes.into());
            self.cache.insert(included_doc.url.clone(), cache_entry);
        }
        debug!("reading includes doneski");
    }

    async fn read_doc_from_path(&self, path: &Path, nested_pos: NestedPos) -> Option<Url> {
        let config = self.config.read().await;
        let Some(file_path) = config.vcl_paths.iter().find_map(|search_root_path| {
            let search_path = search_root_path.join(path);
            if search_path.as_path().exists() {
                Some(search_path)
            } else {
                None
            }
        }) else {
            self.log_error(format!(
                "Could not find {}. Is it in config.vcl_paths?",
                path.to_string_lossy()
            ))
            .await;
            return None;
        };

        let file = match tokio::fs::read_to_string(&file_path).await {
            Ok(file) => file,
            Err(err) => {
                self.log_error(format!(
                    "Failed to read file {} ({err})",
                    path.to_string_lossy()
                ))
                .await;
                return None;
            }
        };

        let doc_url = Url::from_file_path(file_path).unwrap();
        let doc = Document::new(doc_url.clone(), file, Some(nested_pos));
        self.document_map.insert(doc_url.clone(), doc);
        Some(doc_url)
    }

    async fn log_error(&self, message: String) {
        match self.client {
            Some(ref client) => {
                client.log_message(MessageType::ERROR, message).await;
            }
            None => {
                error!("{}", message);
            }
        }
    }
}

impl Default for Backend {
    fn default() -> Self {
        Self {
            client: None,
            document_map: Default::default(),
            root_uri: RwLock::new(
                Url::from_directory_path(std::env::current_dir().unwrap()).unwrap(),
            ),
            root_document_uri: Default::default(),
            config: Default::default(),
            cache: Default::default(),
        }
    }
}
unsafe impl Send for Backend {}
unsafe impl Sync for Backend {}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, init_params: InitializeParams) -> Result<InitializeResult> {
        let root_uri = init_params.root_uri;
        // TODO: consider not initializing if uri scheme is not file

        if let Some(mut root_uri) = root_uri {
            // Fix workspace directory missing slash
            if !root_uri.path().ends_with('/') {
                root_uri.set_path(format!("{}/", root_uri.path()).as_str());
            }
            self.set_root_uri(root_uri.clone()).await;
            if root_uri.scheme() == "file" {
                self.set_config(read_config(&root_uri.to_file_path().unwrap()).await?)
                    .await;
                let config = self.config.read().await;
                if let Some(ref main_vcl_path) = config.main_vcl {
                    if let Some(main_vcl_url) = self.read_doc_from_path(main_vcl_path, vec![]).await
                    {
                        *self.root_document_uri.write().await = Some(main_vcl_url.clone());

                        let includes = {
                            let main_doc = self.document_map.get(&main_vcl_url).unwrap();
                            main_doc.get_includes()
                        };

                        self.read_new_includes(includes).await;
                    }
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
                )),

                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".into(), " ".into(), "(".into()]),
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
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: TextDocumentRegistrationOptions {
                                document_selector: Some(vec![DocumentFilter {
                                    language: Some("vcl".to_string()),
                                    scheme: Some("file".to_string()),
                                    pattern: None,
                                }]),
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: Default::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPES.into(),
                                    token_modifiers: vec!["defaultLibrary".into()],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn shutdown(&self) -> Result<()> {
        std::process::exit(0);
        // Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("did_open({})", params.text_document.uri);
        let uri = params.text_document.uri;
        if let Some(mut doc) = self.document_map.get_mut(&uri) {
            let version = params.text_document.version;
            doc.edit_fulltext(version, params.text_document.text);

            // clear cache
            self.cache.remove(&uri);
        } else {
            let document = Document::new(uri.to_owned(), params.text_document.text, None);
            self.document_map.insert(uri.clone(), document);
        }

        let config = self.config.read().await;
        let doc_includes = {
            let doc = self.document_map.get(&uri).unwrap();
            doc.get_includes()
        };
        self.read_new_includes(doc_includes).await;

        self.client
            .as_ref()
            .unwrap()
            .log_message(MessageType::INFO, "All included files are imported")
            .await;

        let scope = self
            .get_all_definitions_across_all_documents(Some(&uri))
            .await;

        let doc = self.document_map.get(&uri).unwrap();
        self.client
            .as_ref()
            .unwrap()
            .publish_diagnostics(
                uri.clone(),
                doc.diagnostics(scope, &config.lint),
                Some(doc.version()),
            )
            .await;
        debug!("did open done");
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
            let mut doc = self.document_map.get_mut(&uri).unwrap();
            doc.edit(version, changes);
        }

        self.cache.remove(&uri);

        let config = self.config.read().await;

        {
            let doc = self.document_map.get(&uri).unwrap();
            let includes = doc.get_includes();
            drop(doc);
            self.read_new_includes(includes).await;
        }

        let scope = self
            .get_all_definitions_across_all_documents(Some(&uri))
            .await;

        if let Some(doc) = self.document_map.try_get(&uri).try_unwrap() {
            let diagnostics = doc.diagnostics(scope, &config.lint);
            let version = doc.version();
            drop(doc);
            self.client
                .as_ref()
                .unwrap()
                .publish_diagnostics(uri.clone(), diagnostics, Some(version))
                .await;
        }

        debug!("did_change() done!");
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let src_uri = params.text_document_position_params.text_document.uri;
        let src_doc = self.document_map.get(&src_uri).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find source document".into(),
            data: None,
        })?;
        let position = params.text_document_position_params.position;
        let point = Point {
            row: position.line as usize,
            column: position.character as usize,
        };

        let ident = src_doc.get_ident_at_point(point).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find ident".into(),
            data: None,
        })?;

        debug!("goto definition for ident «{}»", ident);

        for doc in self.document_map.iter() {
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
        let doc = self.document_map.get(&src_uri).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find source document".into(),
            data: None,
        })?;
        let position = params.text_document_position.position;
        let point = Point {
            row: position.line as usize,
            column: position.character as usize,
        };

        let ident = doc.get_ident_at_point(point).ok_or(Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: "Could not find ident".into(),
            data: None,
        })?;

        let refs = self
            .document_map
            .iter()
            .flat_map(|doc| doc.get_references_for_ident(ident.as_str()))
            .map(|reference| reference.uri)
            .collect::<Vec<_>>();

        if refs.is_empty() {
            return Ok(None);
        }

        Ok(Some(refs))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let scope = self
            .get_all_definitions_across_all_documents(Some(&uri))
            .await;
        let doc = self.document_map.get(&uri).unwrap();
        debug!("got doc for autocomplete");
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
        let scope = self
            .get_all_definitions_across_all_documents(Some(&uri))
            .await;
        let doc = self.document_map.get(&uri).unwrap();
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
                        func.doc.map(|doc| format!("\n\n{doc}")).unwrap_or_default()
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        debug!("semantic_tokens_full()");
        let start = std::time::Instant::now();
        let uri = params.text_document.uri;
        let Some(doc) = self.document_map.get(&uri) else {
            error!("Could not find document");
            return Err(Error::internal_error());
        };

        let semantic_tokens = doc.get_semantic_tokens();

        debug!(
            "semantic_tokens_full() done in {}ms",
            start.elapsed().as_millis()
        );

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        debug!("document_symbol({})", uri);
        let Some(cache_entry) = self.cache.get(&uri) else {
            error!("Document not found in cache: {}", uri);
            return Ok(None);
        };

        let Some(ref defs) = cache_entry.definitions else {
            return Ok(None);
        };

        Ok(Some(DocumentSymbolResponse::Flat(
            defs.iter()
                .filter(|def| def.loc.is_some())
                .map(|definition| SymbolInformation {
                    name: definition.ident_str.to_string(),
                    kind: match *definition.r#type {
                        Type::Sub => SymbolKind::FUNCTION,
                        Type::Acl => SymbolKind::STRUCT,
                        Type::Backend => SymbolKind::STRUCT,
                        Type::Probe => SymbolKind::STRUCT,
                        Type::Obj(_) => SymbolKind::STRUCT,
                        _ => SymbolKind::NULL,
                    },
                    location: definition.loc.clone().unwrap(),
                    deprecated: None,
                    tags: None,
                    container_name: None,
                })
                .collect(),
        )))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        let Some(doc) = self.document_map.get(&uri) else {
            error!("Could not find document");
            return Err(Error::internal_error());
        };
        let doc_len_lines = doc.rope.len_lines();
        let doc_last_line_len = doc.rope.line(doc_len_lines - 1).len_utf16_cu();
        let src = doc.rope.to_string();
        let config = self.config.read().await;
        let result = crate::formatter::format(src, &config.formatter);
        return Ok(Some(vec![
            TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: doc_len_lines as u32,
                        character: doc_last_line_len as u32,
                    }
                },
                new_text: result,
            }
        ]));
    }
}

pub async fn read_config(root_path: &Path) -> Result<Config> {
    let config_path_buf = root_path.join(PathBuf::from(".varnishls.toml"));
    let config_path = config_path_buf.as_path();
    if !config_path.exists() {
        return Ok(Default::default());
    }
    let config_str = tokio::fs::read_to_string(config_path)
        .await
        .map_err(|err| Error {
            code: tower_lsp::jsonrpc::ErrorCode::InternalError,
            message: err.to_string().into(),
            data: None,
        })?;
    let mut config: Config = toml::from_str(&config_str).map_err(|err| Error {
        code: tower_lsp::jsonrpc::ErrorCode::InternalError,
        message: err.to_string().into(),
        data: None,
    })?;
    config.vcl_paths = config
        .vcl_paths
        .iter()
        .flat_map(|vcl_path| std::fs::canonicalize(vcl_path).ok())
        .collect();
    Ok(config)
}

async fn read_all_vmods(imports: Vec<VmodImport>, config: &Config) -> Definitions {
    let mut definitions = Definitions::default();

    /*
     * read all vcc files by glob (expects e.g. "{vcc_files_dir}/libvmod_std/vmod.vcc")
     *
     * TODO: maybe read all vcc files at startup, cache them, and then lookup from here.
     * TODO: support pointing to singular vcc file or vmod src dir containing a vcc file
     */
    let vcc_files: Vec<PathBuf> = imports
        .iter()
        .flat_map(|import| {
            config
                .vcc_paths
                .iter()
                .find_map(|vcc_path| -> Option<PathBuf> {
                    let vmod_name = &import.name;
                    vec![
                        vcc_path.join(format!("libvmod_{vmod_name}.vcc")),
                        vcc_path.join(format!("vmod_{vmod_name}.vcc")),
                        vcc_path
                            .join(format!("libvmod_{vmod_name}"))
                            .join("vmod.vcc"),
                        vcc_path
                            .join(format!("libvmod_{vmod_name}"))
                            .join(format!("vmod_{vmod_name}.vcc")),
                    ]
                    .into_iter()
                    .find(|path| {
                        let path = Path::new(path);
                        match path.try_exists() {
                            Err(err) => {
                                error!("Could not check {} due to {}", path.display(), err);
                                false
                            }
                            Ok(exists) => exists,
                        }
                    })
                })
        })
        .collect();

    // parse each vcc file in their own thread.
    let mut set = tokio::task::JoinSet::new();
    for vcc_file_path in vcc_files {
        set.spawn(async move { parse_vcc_file_by_path(&vcc_file_path) });
    }

    while let Some(result) = set.join_next().await {
        let result = result
            .map_err(Box::<dyn std::error::Error + Send + Sync>::from) // map JoinError into generic error
            .and_then(|result| result); // replace with result from parse_vcc_file_by_path

        let vmod_scope = match result {
            Err(err) => {
                error!("Failed to parse vmod vcc: {err}");
                continue;
            }
            Ok(vmod_scope) => vmod_scope,
        };
        let vmod_name = match vmod_scope {
            Type::Obj(ref obj) => obj.name.clone(),
            _ => unreachable!(),
        };
        let Some(import) = imports.iter().find(|import| import.name == vmod_name) else {
            error!("Failed to find {vmod_name}. Vmod aliases not yet supported.");
            continue;
        };
        let def = Definition {
            ident_str: vmod_name.clone(),
            r#type: Box::new(vmod_scope),
            loc: Some(import.loc.clone()),
            nested_pos: import.nested_pos.clone(),
        };
        definitions.properties.insert(vmod_name, def);
    }

    // read all vmods
    let vmod_futures = imports
        .iter()
        .filter(|import| !definitions.properties.contains_key(&import.name)) // filter out vmods found by vcc
        .map(|import| {
            // debug!("spawning task to vmod binary for «{vmod_name}»");
            tokio::task::spawn(read_vmod_lib_by_name(
                import.name.clone(),
                config.vmod_paths.to_owned(),
            ))
        })
        .collect::<Vec<_>>();

    for vmod_fut in vmod_futures {
        if let Ok(Ok(Some(vmod))) = vmod_fut.await {
            let vmod_name = vmod.name;
            let Some(import) = imports.iter().find(|import| import.name == vmod_name) else {
                error!("Failed to find {vmod_name}. Vmod aliases not yet supported.");
                continue;
            };
            let def = Definition {
                ident_str: import.name.clone(),
                r#type: Box::new(vmod.scope),
                loc: Some(import.loc.clone()),
                nested_pos: import.nested_pos.clone(),
            };
            definitions.properties.insert(import.name.clone(), def);
        }
    }

    definitions
}

fn get_all_documents(
    doc_map: &DocumentMap,
    cache: &DashMap<Url, CacheEntry>,
    config: &Config,
    doc_uri: &Url,
) -> Vec<Url> {
    let includes: Vec<Include> = {
        let mut entry = cache.entry(doc_uri.clone()).or_default();

        entry
            .includes
            .get_or_insert_with(|| {
                doc_map
                    .get(doc_uri)
                    .map(|doc| {
                        doc.get_includes()
                            .into_iter()
                            .map(|include| include.resolve(&config.vcl_paths))
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default()
            })
            .to_vec()
    };

    let mut deep_includes = includes
        .iter()
        .filter_map(|include| include.url.as_ref())
        .flat_map(|include_url| get_all_documents(doc_map, cache, config, include_url))
        .collect::<Vec<_>>();

    let mut docs = vec![doc_uri.clone()];
    docs.append(&mut deep_includes);
    docs
}
