use std::collections::{BTreeMap, HashMap};
use std::mem::discriminant;
use tower_lsp::lsp_types::Location;

use crate::document::NestedPos;

pub type Properties = BTreeMap<String, Type>;

// implemented by Obj and Definitions
pub trait HasTypeProperties {
    fn get_type_properties_by_range(&self, partial_ident: &str) -> Vec<(&String, &Type)>;
    fn get_type_property(&self, ident: &str) -> Option<&Type>;
    fn obj(&self) -> Option<&Obj>;
}

#[derive(Debug, Clone)]
pub enum Type {
    Obj(Obj),
    Func(Func),
    Backend,
    String,
    Number,
    Duration,
    Time,
    Bool,
    Acl,
    Sub,
    Probe,
    Enum(Vec<String>),
    Blob,
    IP,
    Body,
}

impl Type {
    pub fn is_same_type_as(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }

    pub fn can_this_cast_into(&self, other: &Self) -> bool {
        // anything can cast into string (probably)
        matches!(other, Type::String) ||
            // string can cast into number and ip
            (matches!(self, Type::String) && matches!(other, Type::Number | Type::IP)) ||
            // number can cast into bool
            (matches!(self, Type::Number) && matches!(other, Type::Bool)) ||
            // match same type
            discriminant(self) == discriminant(other)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Obj(_obj) => write!(f, "STRUCT"),
            Type::Func(func) => write!(
                f,
                "{}{}{}",
                match func.ret_type {
                    Some(ref return_str) => format!("{} ", return_str),
                    _ => "".to_string(),
                },
                func.name,
                func.get_signature_string()
            ),
            Type::Backend => write!(f, "BACKEND"),
            Type::String => write!(f, "STRING"),
            Type::Number => write!(f, "NUMBER"),
            Type::Duration => write!(f, "DURATION"),
            Type::Time => write!(f, "TIME"),
            Type::Bool => write!(f, "BOOL"),
            Type::Acl => write!(f, "ACL"),
            Type::Sub => write!(f, "SUBROUTINE"),
            Type::Probe => write!(f, "PROBE"),
            Type::Enum(values) => write!(f, "ENUM {{{}}}", values.join(", ")),
            Type::Blob => write!(f, "BLOB"),
            Type::IP => write!(f, "IP"),
            Type::Body => write!(f, "BODY"),
        }
    }
}

// top level scope
#[derive(Debug, Default)]
pub struct Definitions {
    pub properties: BTreeMap<String, Definition>,
}

impl Definitions {
    /// alias to default for now
    pub fn new() -> Definitions {
        Definitions::default()
    }
}

pub struct AutocompleteSearchOptions {
    pub search_type: Option<Type>,
    // pub ignore_type: Option<Type>,
    pub must_be_writable: Option<bool>,
}

impl Definitions {
    pub fn get(&self, ident: &str) -> Option<&Definition> {
        self.properties.get(ident)
    }

    pub fn get_type_property_by_nested_idents(&self, idents: Vec<&str>) -> Option<&Type> {
        let mut scope: &dyn HasTypeProperties = self;
        let (idents, [last_ident, ..]) = idents.split_at(idents.len() - 1) else {
            unreachable!("Failed to split identifier by period");
        };
        for ident in idents {
            let Some(Type::Obj(ref obj)) = scope.get_type_property(ident) else {
                return None;
            };
            scope = obj;
        }
        scope.get_type_property(last_ident)
    }

    pub fn get_type_properties_by_idents(
        &self,
        idents: Vec<&str>,
        options: AutocompleteSearchOptions,
    ) -> Option<Vec<(&String, &Type)>> {
        let mut scope: &dyn HasTypeProperties = self;
        let (idents, [last_ident, ..]) = idents.split_at(idents.len() - 1) else {
            unreachable!("Failed to split identifier by period");
        };
        for ident in idents {
            let Some(Type::Obj(ref obj)) = scope.get_type_property(ident) else {
                return None;
            };
            scope = obj;
        }

        Some(
            scope
                .get_type_properties_by_range(last_ident)
                .iter()
                .filter(|(_prop_name, property)| {
                    if let Some(ref search_type) = options.search_type {
                        scope_contains_type(property, search_type, true)
                    } else if options.must_be_writable.unwrap_or(false) {
                        let is_writable = scope.obj().map_or(false, |obj| !obj.read_only);
                        is_writable || scope_contains_writable(property)
                    } else {
                        // match on everything
                        true
                    }
                })
                .map(|(a, b)| (*a, *b)) // hmm
                .collect::<Vec<_>>(),
        )
    }
}

impl HasTypeProperties for Definitions {
    fn get_type_properties_by_range(&self, partial_ident: &str) -> Vec<(&String, &Type)> {
        self.properties
            .range(partial_ident.to_string()..)
            .take_while(|(key, _v)| key.starts_with(partial_ident))
            .map(|(name, def)| (name, &(*def.r#type)))
            .collect()
    }
    fn get_type_property(&self, ident: &str) -> Option<&Type> {
        self.properties.get(ident).map(|def| &(*def.r#type))
    }
    fn obj(&self) -> Option<&Obj> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub ident_str: String,
    pub r#type: Box<Type>,
    // TODO: replace line_num and doc_url with a Location?
    // pub line_num: usize,
    // pub doc_url: Option<String>,
    pub loc: Option<Location>,
    pub nested_pos: NestedPos,
}

impl Definition {
    pub fn new_builtin(ident_str: String, r#type: Type) -> Definition {
        Definition {
            ident_str,
            r#type: Box::new(r#type),
            loc: None,
            nested_pos: Default::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Obj {
    pub name: String,
    pub properties: Properties,
    pub read_only: bool,
    pub definition: Option<Definition>,
    pub is_http_headers: bool,
}

impl HasTypeProperties for Obj {
    fn get_type_properties_by_range(&self, partial_ident: &str) -> Vec<(&String, &Type)> {
        self.properties
            .range(partial_ident.to_string()..)
            .take_while(|(key, _v)| key.starts_with(partial_ident))
            .collect()
    }
    fn get_type_property(&self, ident: &str) -> Option<&Type> {
        if self.is_http_headers {
            return Some(&Type::String);
        }
        self.properties.get(ident)
    }
    fn obj(&self) -> Option<&Obj> {
        Some(self)
    }
}

#[derive(Debug, Clone, Default)]
pub struct FuncArg {
    pub name: Option<String>, // None if not named
    pub optional: bool,
    pub r#type: Option<Type>,
    pub default_value: Option<String>,
}

#[derive(Debug, Default, Clone)]
pub struct Func {
    pub name: String,
    pub definition: Option<Definition>,
    pub ret_type: Option<String>, // TEMP
    pub r#return: Option<Box<Type>>,
    pub doc: Option<String>,
    pub args: Vec<FuncArg>,
    pub restricted: Option<Vec<String>>,
}

impl Func {
    pub fn get_signature_string(&self) -> String {
        format!(
            "({})",
            self.args
                .iter()
                .filter_map(|arg| {
                    let mut str = String::new();
                    if let Some(ref r#type) = arg.r#type {
                        str.push_str(format!("{}", r#type).as_str());
                    }
                    if let Some(ref arg_name) = arg.name {
                        str.push_str(format!(" {}", arg_name).as_str());
                    }
                    if let Some(ref default_value) = arg.default_value {
                        str.push_str(format!(" = {}", default_value).as_str());
                    }
                    if arg.optional {
                        str = format!("[{}]", str);
                    }

                    if !str.is_empty() {
                        Some(str)
                    } else {
                        None
                    }
                })
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

const DEFAULT_REQUEST_HEADERS: &[&str] = &[
    "host",
    "origin",
    "cookie",
    "user-agent",
    "referer",
    "if-none-match",
    "if-modified-since",
    "accept",
    "authorization",
];

const DEFAULT_RESPONSE_HEADERS: &[&str] = &[
    "vary",
    "origin",
    "server",
    "age",
    "expires",
    "etag",
    "last-modified",
    "content-type",
    "cache-control",
    "surrogate-control",
    "location",
    "set-cookie",
];

// https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vcc_backend.c#L121-L130
pub const PROBE_FIELDS: &[&str] = &[
    "url",
    "request",
    "expected_response",
    "timeout",
    "interval",
    "window",
    "threshold",
    "initial",
];

pub fn get_probe_field_types<'a>() -> HashMap<&'a str, Type> {
    HashMap::from([
        ("url", Type::String),
        ("request", Type::String),
        ("expected_response", Type::Number),
        ("timeout", Type::Duration),
        ("interval", Type::Duration),
        ("window", Type::Number),
        ("threshold", Type::Number),
        ("initial", Type::Number),
    ])
}

// https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vcc_backend.c#L311-L322
pub const BACKEND_FIELDS: &[&str] = &[
    "host",
    "port",
    "path",
    "host_header",
    "connect_timeout",
    "first_byte_timeout",
    "between_bytes_timeout",
    "probe",
    "max_connections",
    "proxy_header",
];

pub fn get_backend_field_types<'a>() -> HashMap<&'a str, Type> {
    HashMap::from([
        ("host", Type::String),
        ("port", Type::Number), // can be string
        ("path", Type::String),
        ("host_header", Type::String),
        ("connect_timeout", Type::Duration),
        ("first_byte_timeout", Type::Duration),
        ("between_bytes_timeout", Type::Duration),
        ("probe", Type::Probe),
        ("max_connections", Type::String),
        ("proxy_header", Type::String),
        // 7.0 fields
        ("via", Type::Backend),
        ("preamble", Type::Blob),
        ("authority", Type::String),
    ])
}

pub const RETURN_METHODS: &[&str] = &[
    "hit", "miss", "pass", "pipe", "retry", "restart", "fail", "synth", "hash", "deliver",
    "abandon", "lookup", "error", "purge",
];
pub fn get_varnish_builtins() -> Definitions {
    let req: Type = Type::Obj(Obj {
        name: "req".to_string(),
        read_only: false,
        properties: BTreeMap::from([
            (
                "http".to_string(),
                Type::Obj(Obj {
                    name: "req.http".to_string(),
                    read_only: false,
                    is_http_headers: true,
                    properties: BTreeMap::from_iter(
                        DEFAULT_REQUEST_HEADERS
                            .iter()
                            .map(|header| (header.to_string(), Type::String)),
                    ),
                    ..Obj::default()
                }),
            ),
            ("url".to_string(), Type::String),
            ("method".to_string(), Type::String),
            // ("hash".to_string(), Type::String), // varnish 3?
            ("proto".to_string(), Type::String),
            ("backend_hint".to_string(), Type::Backend),
            ("restarts".to_string(), Type::Number),
            ("ttl".to_string(), Type::Duration),
            ("grace".to_string(), Type::Duration),
            ("is_hitmiss".to_string(), Type::Bool),
            ("is_hitpass".to_string(), Type::Bool),
            ("do_esi".to_string(), Type::Bool),
            ("can_gzip".to_string(), Type::Bool),
            ("hash_ignore_busy".to_string(), Type::Bool),
            ("hash_always_miss".to_string(), Type::Bool),
            ("xid".to_string(), Type::String),
        ]),
        ..Obj::default()
    });

    let bereq: Type = Type::Obj(Obj {
        name: "bereq".to_string(),
        read_only: false,
        properties: BTreeMap::from([
            (
                "http".to_string(),
                Type::Obj(Obj {
                    name: "bereq.http".to_string(),
                    read_only: false,
                    is_http_headers: true,
                    properties: BTreeMap::from_iter(
                        DEFAULT_REQUEST_HEADERS
                            .iter()
                            .map(|header| (header.to_string(), Type::String)),
                    ),
                    ..Obj::default()
                }),
            ),
            ("url".to_string(), Type::String),
            ("method".to_string(), Type::String),
            ("xid".to_string(), Type::String),
            ("retries".to_string(), Type::Number),
            // ("hash".to_string(), Type::String),
            ("proto".to_string(), Type::String),
            ("backend".to_string(), Type::Backend),
            ("uncacheable".to_string(), Type::Bool),
            ("is_bgfetch".to_string(), Type::Bool),
            ("body".to_string(), Type::Body),
        ]),
        ..Obj::default()
    });

    let resp: Type = Type::Obj(Obj {
        name: "resp".to_string(),
        read_only: false,
        properties: BTreeMap::from([
            (
                "http".to_string(),
                Type::Obj(Obj {
                    name: "req.http".to_string(),
                    read_only: false,
                    is_http_headers: true,
                    properties: BTreeMap::from_iter(
                        DEFAULT_RESPONSE_HEADERS
                            .iter()
                            .map(|header| (header.to_string(), Type::String)),
                    ),
                    ..Obj::default()
                }),
            ),
            ("status".to_string(), Type::Number),
            ("reason".to_string(), Type::String),
            ("backend".to_string(), Type::Backend),
            ("is_streaming".to_string(), Type::Bool),
            ("body".to_string(), Type::Body),
        ]),
        ..Obj::default()
    });

    let beresp: Type = Type::Obj(Obj {
        name: "beresp".to_string(),
        read_only: false,
        properties: BTreeMap::from([
            (
                "http".to_string(),
                Type::Obj(Obj {
                    name: "req.http".to_string(),
                    read_only: false,
                    is_http_headers: true,
                    properties: BTreeMap::from_iter(
                        DEFAULT_RESPONSE_HEADERS
                            .iter()
                            .map(|header| (header.to_string(), Type::String)),
                    ),
                    ..Obj::default()
                }),
            ),
            ("status".to_string(), Type::Number),
            ("reason".to_string(), Type::String),
            ("backend".to_string(), Type::Backend),
            ("backend.name".to_string(), Type::String),
            ("backend.ip".to_string(), Type::String),
            ("uncacheable".to_string(), Type::Bool),
            ("age".to_string(), Type::Duration),
            ("ttl".to_string(), Type::Duration),
            ("grace".to_string(), Type::Duration),
            ("keep".to_string(), Type::Duration),
            // ("storage".to_string(), Type::String),
            ("storage_hint".to_string(), Type::String),
            ("body".to_string(), Type::Body),
        ]),
        ..Obj::default()
    });

    let obj: Type = Type::Obj(Obj {
        name: "obj".to_string(),
        read_only: false,
        properties: BTreeMap::from([
            ("ttl".to_string(), Type::Duration),
            ("grace".to_string(), Type::Duration),
            ("keep".to_string(), Type::Duration),
            ("age".to_string(), Type::Duration),
            ("hits".to_string(), Type::Number),
            ("uncacheable".to_string(), Type::Bool),
            (
                "http".to_string(),
                Type::Obj(Obj {
                    name: "obj.http".to_string(),
                    read_only: false,
                    is_http_headers: true,
                    ..Default::default()
                }),
            ),
        ]),
        ..Obj::default()
    });

    let sess: Type = Type::Obj(Obj {
        name: "sess".to_string(),
        properties: BTreeMap::from([
            ("timeout_idle".to_string(), Type::Duration),
            ("xid".to_string(), Type::String),
        ]),
        ..Obj::default()
    });

    let client: Type = Type::Obj(Obj {
        name: "client".to_string(),
        read_only: true,
        properties: BTreeMap::from([
            ("ip".to_string(), Type::String),
            ("identity".to_string(), Type::String),
        ]),
        ..Obj::default()
    });

    let server: Type = Type::Obj(Obj {
        name: "server".to_string(),
        read_only: true,
        properties: BTreeMap::from([
            ("ip".to_string(), Type::String),
            ("hostname".to_string(), Type::String),
            ("identity".to_string(), Type::String),
        ]),
        ..Obj::default()
    });

    let local: Type = Type::Obj(Obj {
        name: "local".to_string(),
        read_only: true,
        properties: BTreeMap::from([
            ("ip".to_string(), Type::String),
            ("endpoint".to_string(), Type::String),
            ("socket".to_string(), Type::String),
        ]),
        ..Obj::default()
    });

    let remote: Type = Type::Obj(Obj {
        name: "remote".to_string(),
        read_only: true,
        properties: BTreeMap::from([("ip".to_string(), Type::String)]),
        ..Obj::default()
    });

    let regsub = Type::Func(Func {
        name: "regsub".to_string(),
        args: vec![
            FuncArg {
                r#type: Some(Type::String),
                name: Some("str".into()),
                ..Default::default()
            },
            FuncArg {
                r#type: Some(Type::String),
                name: Some("regex".into()),
                ..Default::default()
            },
            FuncArg {
                r#type: Some(Type::String),
                name: Some("sub".into()),
                ..Default::default()
            },
        ],
        r#return: Some(Box::new(Type::String)),
        ..Func::default()
    });

    let regsuball = Type::Func(Func {
        name: "regsuball".to_string(),
        args: vec![
            FuncArg {
                name: Some("str".into()),
                r#type: Some(Type::String),
                ..Default::default()
            },
            FuncArg {
                name: Some("regex".into()),
                r#type: Some(Type::String),
                ..Default::default()
            },
            FuncArg {
                name: Some("sub".into()),
                r#type: Some(Type::String),
                ..Default::default()
            },
        ],
        r#return: Some(Box::new(Type::String)),
        ..Func::default()
    });

    let synthetic = Type::Func(Func {
        name: "synthetic".to_string(),
        args: vec![FuncArg {
            name: Some("str".into()),
            r#type: Some(Type::String),
            ..Default::default()
        }],
        ..Func::default()
    });

    let hash_data = Type::Func(Func {
        name: "hash_data".to_string(),
        args: vec![FuncArg {
            name: Some("str".into()),
            r#type: Some(Type::String),
            ..Default::default()
        }],
        ..Func::default()
    });

    let ban = Type::Func(Func {
        name: "ban".to_string(),
        args: vec![FuncArg {
            name: Some("str".into()),
            r#type: Some(Type::String),
            ..Default::default()
        }],
        ..Func::default()
    });

    let now = Type::Time;

    Definitions {
        #[rustfmt::skip]
        properties: BTreeMap::from([
            ("req".into(),       Definition::new_builtin("req".into(),       req      )),
            ("bereq".into(),     Definition::new_builtin("bereq".into(),     bereq    )),
            ("resp".into(),      Definition::new_builtin("resp".into(),      resp     )),
            ("beresp".into(),    Definition::new_builtin("beresp".into(),    beresp   )),
            ("obj".into(),       Definition::new_builtin("obj".into(),       obj      )),
            ("sess".into(),      Definition::new_builtin("sess".into(),      sess     )),
            ("client".into(),    Definition::new_builtin("client".into(),    client   )),
            ("server".into(),    Definition::new_builtin("server".into(),    server   )),
            ("local".into(),     Definition::new_builtin("local".into(),     local    )),
            ("remote".into(),    Definition::new_builtin("remote".into(),    remote   )),
            ("regsub".into(),    Definition::new_builtin("regsub".into(),    regsub   )),
            ("regsuball".into(), Definition::new_builtin("regsuball".into(), regsuball)),
            ("synthetic".into(), Definition::new_builtin("synthetic".into(), synthetic)),
            ("hash_data".into(), Definition::new_builtin("hash_data".into(), hash_data)),
            ("ban".into(),       Definition::new_builtin("ban".into(),       ban)),
            ("now".into(),       Definition::new_builtin("now".into(),       now)),
        ]),
    }
}

/*
 * Check if provided `scope` contains provided type (`type_to_compare`). can_this_turn_into means
 * checking whether anything in `scope` can turn (cast) into `type_to_compare`.
 */
pub fn scope_contains_type(scope: &Type, type_to_compare: &Type, can_this_turn_into: bool) -> bool {
    if can_this_turn_into {
        if scope.can_this_cast_into(type_to_compare) {
            return true;
        }
    } else if scope.is_same_type_as(type_to_compare) {
        return true;
    }

    match scope {
        Type::Obj(obj) => obj
            .properties
            .values()
            .any(|prop| scope_contains_type(prop, type_to_compare, can_this_turn_into)),
        Type::Func(func) => match func.r#return {
            Some(ref ret_type) => {
                scope_contains_type(ret_type, type_to_compare, can_this_turn_into)
            }
            _ => false,
        },
        _ => false,
    }
}

pub fn scope_contains_writable(scope: &Type) -> bool {
    match scope {
        Type::Obj(obj) => !obj.read_only || obj.properties.values().any(scope_contains_writable),
        _ => false,
    }
}
