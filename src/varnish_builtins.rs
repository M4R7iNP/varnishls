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
    Bytes,
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
            Type::Bytes => write!(f, "BYTES"),
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
    pub doc: Option<String>, // Added field for documentation
}

impl Definition {
    pub fn new_builtin(ident_str: String, r#type: Type, doc: Option<String>) -> Definition {
        Definition {
            ident_str,
            r#type: Box::new(r#type),
            loc: None,
            nested_pos: vec![], // Changed from NestedPos::Builtin
            doc,
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
        // Varnish Plus
        ("tcponly", Type::Number),
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
    "last_byte_timeout", // varnish enterprise
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
        ("last_byte_timeout", Type::Duration),
        ("probe", Type::Probe),
        ("max_connections", Type::String),
        ("proxy_header", Type::String),
        // 7.0 fields
        ("via", Type::Backend),
        ("preamble", Type::Blob),
        ("authority", Type::String),
        // Varnish Plus Backend SSL/TLS fields
        ("ssl", Type::Number),
        ("ssl_sni", Type::Number),
        ("ssl_verify_peer", Type::Number),
        ("ssl_verify_host", Type::Number),
    ])
}

pub const RETURN_METHODS: &[&str] = &[
    "hit", "miss", "pass", "pipe", "retry", "restart", "fail", "synth", "hash", "deliver",
    "abandon", "lookup", "error", "purge",
];
pub fn get_varnish_builtins() -> Definitions {
    let mut req_http_headers = Obj {
        name: "http".into(),
        is_http_headers: true,
        properties: Properties::new(),
        read_only: false,
        definition: Some(Definition::new_builtin(
            "req.http".into(),
            Type::String, // This is a simplification, as it can be various types.
            Some("The headers of request, things like req.http.date.".to_string()),
        )),
    };
    for header in DEFAULT_REQUEST_HEADERS.iter() {
        req_http_headers.properties.insert(
            header.to_string(),
            Type::Obj(Obj {
                name: header.to_string(),
                read_only: false,
                is_http_headers: false, // Individual headers are not themselves header collections
                definition: Some(Definition::new_builtin(
                    format!("req.http.{}", header),
                    Type::String,
                    None, // Placeholder for individual header documentation
                )),
                ..Default::default()
            }),
        );
    }

    let req = Type::Obj(Obj {
        name: "req".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "req".into(),
            Type::String, // Simplified
            Some("These variables describe the present request".to_string()),
        )),
        properties: HashMap::from([
            (
                "ttl".into(),
                Type::Obj(Obj {
                    name: "ttl".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.ttl".into(),
                        Type::Duration,
                        Some("Upper limit on the object age for cache lookups to return hit.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "grace".into(),
                Type::Obj(Obj {
                    name: "grace".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.grace".into(),
                        Type::Duration,
                        Some("Upper limit on the object grace.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "xid".into(),
                Type::Obj(Obj {
                    name: "xid".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.xid".into(),
                        Type::String,
                        Some("Unique ID of this request.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "restarts".into(),
                Type::Obj(Obj {
                    name: "restarts".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.restarts".into(),
                        Type::Number,
                        Some("A count of how many times this request has been restarted.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "backend_hint".into(),
                Type::Obj(Obj {
                    name: "backend_hint".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.backend_hint".into(),
                        Type::Backend,
                        Some("Set bereq.backend to this if we attempt to fetch.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "hash_ignore_busy".into(),
                Type::Obj(Obj {
                    name: "hash_ignore_busy".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.hash_ignore_busy".into(),
                        Type::Bool,
                        Some("Ignore any busy object during cache lookup.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "hash_always_miss".into(),
                Type::Obj(Obj {
                    name: "hash_always_miss".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.hash_always_miss".into(),
                        Type::Bool,
                        Some("Ignore regular cache lookups and mark the request as a miss.".to_string()), // Doc from common knowledge / general Varnish behavior
                    )),
                    ..Default::default()
                }),
            ),
            (
                "can_gzip".into(),
                Type::Obj(Obj {
                    name: "can_gzip".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.can_gzip".into(),
                        Type::Bool,
                        Some("True if the client provided gzip or x-gzip in the Accept-Encoding header.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "method".into(),
                Type::Obj(Obj {
                    name: "method".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.method".into(),
                        Type::String,
                        Some("The request method (e.g. \"GET\", \"HEAD\", ...).".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "url".into(),
                Type::Obj(Obj {
                    name: "url".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.url".into(),
                        Type::String,
                        Some("The requested URL, for instance \"/robots.txt\".".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "proto".into(),
                Type::Obj(Obj {
                    name: "proto".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.proto".into(),
                        Type::String,
                        Some("The HTTP protocol version used by the client, usually \"HTTP/1.1\" or \"HTTP/2.0\".".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            (
                "esi_level".into(),
                Type::Obj(Obj {
                    name: "esi_level".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "req.esi_level".into(),
                        Type::Number,
                        Some("A count of how many levels of ESI requests we're currently at.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            // req.http.Header
            ("http".into(), Type::Obj(req_http_headers)),
        ])
        .into_iter()
        .collect(),
    });

    let mut bereq_http_headers = Obj {
        name: "http".into(),
        is_http_headers: true,
        properties: Properties::new(),
        read_only: false,
        definition: Some(Definition::new_builtin(
            "bereq.http".into(),
            Type::String, // Simplified
            Some("The headers of the backend request.".to_string()),
        )),
    };
    for header in DEFAULT_REQUEST_HEADERS.iter() { // Should ideally be DEFAULT_BACKEND_REQUEST_HEADERS if different
        bereq_http_headers.properties.insert(
            header.to_string(),
            Type::Obj(Obj {
                name: header.to_string(),
                read_only: false,
                is_http_headers: false,
                definition: Some(Definition::new_builtin(
                    format!("bereq.http.{}", header),
                    Type::String,
                    None, // Placeholder
                )),
                ..Default::default()
            }),
        );
    }

    let bereq = Type::Obj(Obj {
        name: "bereq".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "bereq".into(),
            Type::String, // Simplified
            Some("This is the request we send to the backend".to_string()),
        )),
        properties: HashMap::from([
            (
                "xid".into(),
                Type::Obj(Obj {
                    name: "xid".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "bereq.xid".into(),
                        Type::String,
                        Some("Unique ID of this backend request.".to_string()), // Doc from common knowledge
                    )),
                    ..Default::default()
                }),
            ),
            // bereq.http.Header
            ("http".into(), Type::Obj(bereq_http_headers)),
        ])
        .into_iter()
        .collect(),
    });

    let mut req_top_http_headers = Obj {
        name: "http".into(),
        is_http_headers: true,
        properties: Properties::new(),
        read_only: false,
        definition: Some(Definition::new_builtin(
            "req_top.http".into(),
            Type::String, // Simplified
            Some("HTTP headers of the top-level request in a tree of ESI requests.".to_string()),
        )),
    };
    for header in DEFAULT_REQUEST_HEADERS.iter() {
        req_top_http_headers.properties.insert(
            header.to_string(),
            Type::Obj(Obj {
                name: header.to_string(),
                read_only: false,
                is_http_headers: false,
                definition: Some(Definition::new_builtin(
                    format!("req_top.http.{}", header),
                    Type::String,
                    None, // Placeholder
                )),
                ..Default::default()
            }),
        );
    }

    let req_top = Type::Obj(Obj {
        name: "req_top".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "req_top".into(),
            Type::String, // Simplified
            Some("When ESI:include requests are being processed, req_top points to the request received from the client.".to_string()),
        )),
        properties: HashMap::from([
            // req_top.http.Header
            ("http".into(), Type::Obj(req_top_http_headers)),
        ])
        .into_iter()
        .collect(),
    });

    let mut resp_http_headers = Obj {
        name: "http".into(),
        is_http_headers: true,
        properties: Properties::new(),
        read_only: false,
        definition: Some(Definition::new_builtin(
            "resp.http".into(),
            Type::String, // Simplified
            Some("The headers of the response sent to the client.".to_string()),
        )),
    };
    for header in DEFAULT_RESPONSE_HEADERS.iter() {
        resp_http_headers.properties.insert(
            header.to_string(),
            Type::Obj(Obj {
                name: header.to_string(),
                read_only: false,
                is_http_headers: false,
                definition: Some(Definition::new_builtin(
                    format!("resp.http.{}", header),
                    Type::String,
                    None, // Placeholder
                )),
                ..Default::default()
            }),
        );
    }

    let resp = Type::Obj(Obj {
        name: "resp".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "resp".into(),
            Type::String, // Simplified
            Some("This is the response we send to the client".to_string()),
        )),
        properties: HashMap::from([
            // resp.http.Header
            ("http".into(), Type::Obj(resp_http_headers)),
        ])
        .into_iter()
        .collect(),
    });

    let mut beresp_http_headers = Obj {
        name: "http".into(),
        is_http_headers: true,
        properties: Properties::new(),
        read_only: false,
        definition: Some(Definition::new_builtin(
            "beresp.http".into(),
            Type::String, // Simplified
            Some("The headers of the response received from the backend.".to_string()),
        )),
    };
    for header in DEFAULT_RESPONSE_HEADERS.iter() {
        beresp_http_headers.properties.insert(
            header.to_string(),
            Type::Obj(Obj {
                name: header.to_string(),
                read_only: false,
                is_http_headers: false,
                definition: Some(Definition::new_builtin(
                    format!("beresp.http.{}", header),
                    Type::String,
                    None, // Placeholder
                )),
                ..Default::default()
            }),
        );
    }

    let beresp = Type::Obj(Obj {
        name: "beresp".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "beresp".into(),
            Type::String, // Simplified
            Some("The response received from the backend".to_string()),
        )),
        properties: HashMap::from([
            (
                "ttl".into(),
                Type::Obj(Obj {
                    name: "ttl".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "beresp.ttl".into(),
                        Type::Duration,
                        Some("The object's remaining time to live (TTL) from the backend.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "grace".into(),
                Type::Obj(Obj {
                    name: "grace".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "beresp.grace".into(),
                        Type::Duration,
                        Some("The object's grace period from the backend.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "uncacheable".into(),
                Type::Obj(Obj {
                    name: "uncacheable".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "beresp.uncacheable".into(),
                        Type::Bool,
                        Some("If true, the object is marked as uncacheable.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "backend".into(),
                Type::Obj(Obj {
                    name: "backend".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "beresp.backend".into(),
                        Type::Backend,
                        Some("The backend that served this response.".to_string()), // Doc from vcl_var.rst
                    )),
                    properties: HashMap::from([
                        (
                            "name".into(),
                            Type::Obj(Obj {
                                name: "name".into(),
                                read_only: true,
                                is_http_headers: false,
                                definition: Some(Definition::new_builtin(
                                    "beresp.backend.name".into(),
                                    Type::String,
                                    Some("The name of the backend.".to_string()), // Doc from vcl_var.rst
                                )),
                                ..Default::default()
                            }),
                        ),
                        (
                            "ip".into(),
                            Type::Obj(Obj {
                                name: "ip".into(),
                                read_only: true,
                                is_http_headers: false,
                                definition: Some(Definition::new_builtin(
                                    "beresp.backend.ip".into(),
                                    Type::IP,
                                    Some("The IP address of the backend.".to_string()), // Doc from vcl_var.rst
                                )),
                                ..Default::default()
                            }),
                        ),
                    ])
                    .into_iter()
                    .collect(),
                }),
            ),
            // beresp.http.Header
            ("http".into(), Type::Obj(beresp_http_headers)),
        ])
        .into_iter()
        .collect(),
    });

    let mut obj_http_headers = Obj {
        name: "http".into(),
        is_http_headers: true,
        properties: Properties::new(),
        read_only: true,
        definition: Some(Definition::new_builtin(
            "obj.http".into(),
            Type::String, // Simplified
            Some("HTTP headers of the cached object.".to_string()),
        )),
    };
    for header in DEFAULT_RESPONSE_HEADERS.iter() {
        obj_http_headers.properties.insert(
            header.to_string(),
            Type::Obj(Obj {
                name: header.to_string(),
                read_only: true,
                is_http_headers: false,
                definition: Some(Definition::new_builtin(
                    format!("obj.http.{}", header),
                    Type::String,
                    None, // Placeholder
                )),
                ..Default::default()
            }),
        );
    }

    let obj = Type::Obj(Obj {
        name: "obj".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "obj".into(),
            Type::String, // Simplified
            Some("This is the object we found in cache. It cannot be modified.".to_string()),
        )),
        properties: HashMap::from([
            (
                "hits".into(),
                Type::Obj(Obj {
                    name: "hits".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "obj.hits".into(),
                        Type::Number,
                        Some("How many times this object has been delivered.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "ttl".into(),
                Type::Obj(Obj {
                    name: "ttl".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "obj.ttl".into(),
                        Type::Duration,
                        Some("The object's remaining time to live (TTL) as stored in cache.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "grace".into(),
                Type::Obj(Obj {
                    name: "grace".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "obj.grace".into(),
                        Type::Duration,
                        Some("The object's grace period as stored in cache.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "keep".into(),
                Type::Obj(Obj {
                    name: "keep".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "obj.keep".into(),
                        Type::Duration,
                        Some("How long Varnish will attempt to keep the object in cache.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "lastuse".into(), // Not in vcl_var.rst, but common knowledge / older versions
                Type::Obj(Obj {
                    name: "lastuse".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "obj.lastuse".into(),
                        Type::Duration,
                        Some("Time since the object was last used.".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
            // obj.http.Header
            ("http".into(), Type::Obj(obj_http_headers)),
        ])
        .into_iter()
        .collect(),
    });

    let sess = Type::Obj(Obj {
        name: "sess".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "sess".into(),
            Type::String, // Simplified
            Some("A session corresponds to the \"conversation\" that Varnish has with a single client connection".to_string()),
        )),
        properties: HashMap::from([
            (
                "xid".into(),
                Type::Obj(Obj {
                    name: "xid".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "sess.xid".into(),
                        Type::String,
                        Some("Unique ID of this session.".to_string()), // Doc from vcl_var.rst (VCL >= 4.1)
                    )),
                    ..Default::default()
                }),
            ),
            (
                "timeout_idle".into(),
                Type::Obj(Obj {
                    name: "timeout_idle".into(),
                    read_only: false,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "sess.timeout_idle".into(),
                        Type::Duration,
                        Some("Idle timeout for the session.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
        ])
        .into_iter()
        .collect(),
    });

    let client = Type::Obj(Obj {
        name: "client".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "client".into(),
            Type::String, // Simplified
            Some("These variables describe the network connection between the client and varnishd.".to_string()), // Doc from vcl_var.rst
        )),
        properties: HashMap::from([
            (
                "ip".into(),
                Type::Obj(Obj {
                    name: "ip".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "client.ip".into(),
                        Type::IP,
                        Some("The client's IP address.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "identity".into(),
                Type::Obj(Obj {
                    name: "identity".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "client.identity".into(),
                        Type::String,
                        Some("Client identity if PROXY protocol is used.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
        ])
        .into_iter()
        .collect(),
    });

    let server = Type::Obj(Obj {
        name: "server".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "server".into(),
            Type::String, // Simplified
            Some("These variables describe the network connection on the Varnish server side.".to_string()), // Doc from vcl_var.rst
        )),
        properties: HashMap::from([
            (
                "ip".into(),
                Type::Obj(Obj {
                    name: "ip".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "server.ip".into(),
                        Type::IP,
                        Some("The IP address Varnish is listening on for this request.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "hostname".into(),
                Type::Obj(Obj {
                    name: "hostname".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "server.hostname".into(),
                        Type::String,
                        Some("The hostname of the Varnish server.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
            (
                "identity".into(),
                Type::Obj(Obj {
                    name: "identity".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "server.identity".into(),
                        Type::String,
                        Some("The identity of the Varnish server.".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
        ])
        .into_iter()
        .collect(),
    });

    let local = Type::Obj(Obj {
        name: "local".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "local".into(),
            Type::String, // Simplified
            Some("Describes the local end of the TCP connection.".to_string()), // Doc from vcl_var.rst
        )),
        properties: HashMap::from([
            (
                "ip".into(),
                Type::Obj(Obj {
                    name: "ip".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "local.ip".into(),
                        Type::IP,
                        Some("The IP address (and port number) of the local end of the TCP connection".to_string()),
                    )),
                    ..Default::default()
                }),
            ),
        ])
        .into_iter()
        .collect(),
    });

    let remote = Type::Obj(Obj {
        name: "remote".into(),
        read_only: true,
        is_http_headers: false,
        definition: Some(Definition::new_builtin(
            "remote".into(),
            Type::String, // Simplified
            Some("Describes the remote end of the TCP connection (client or proxy).".to_string()), // Doc from vcl_var.rst
        )),
        properties: HashMap::from([
            (
                "ip".into(),
                Type::Obj(Obj {
                    name: "ip".into(),
                    read_only: true,
                    is_http_headers: false,
                    definition: Some(Definition::new_builtin(
                        "remote.ip".into(),
                        Type::IP,
                        Some("The IP address of the remote end (client or proxy).".to_string()), // Doc from vcl_var.rst
                    )),
                    ..Default::default()
                }),
            ),
        ])
        .into_iter()
        .collect(),
    });

    let regsub = Type::Func(Func {
        name: "regsub".into(),
        r#return: Some(Box::new(Type::String)),
        doc: Some("Replaces the first occurrence of a regular expression in a string.".to_string()),
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
        definition: Some(Definition::new_builtin(
            "regsub".into(),
            Type::String, // Simplified, this is a function
            Some("Replaces the first occurrence of a regular expression in a string.".to_string()),
        )),
        ..Default::default()
    });

    let regsuball = Type::Func(Func {
        name: "regsuball".into(),
        r#return: Some(Box::new(Type::String)),
        doc: Some("Replaces all occurrences of a regular expression in a string.".to_string()),
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
        definition: Some(Definition::new_builtin(
            "regsuball".into(),
            Type::String, // Simplified, this is a function
            Some("Replaces all occurrences of a regular expression in a string.".to_string()),
        )),
        ..Default::default()
    });

    let synthetic = Type::Func(Func {
        name: "synthetic".into(),
        doc: Some("Generates a synthetic response.".to_string()),
        args: vec![FuncArg {
            name: Some("str".into()),
            r#type: Some(Type::String),
            ..Default::default()
        }],
        definition: Some(Definition::new_builtin(
            "synthetic".into(),
            Type::String, // Simplified, this is a function
            Some("Generates a synthetic response.".to_string()),
        )),
        ..Default::default()
    });

    let hash_data = Type::Func(Func {
        name: "hash_data".into(),
        doc: Some("Adds data to the hash for object lookup.".to_string()),
        args: vec![FuncArg {
            name: Some("str".into()),
            r#type: Some(Type::String),
            ..Default::default()
        }],
        definition: Some(Definition::new_builtin(
            "hash_data".into(),
            Type::String, // Simplified, this is a function
            Some("Adds data to the hash for object lookup.".to_string()),
        )),
        ..Default::default()
    });

    let ban = Type::Func(Func {
        name: "ban".into(),
        doc: Some("Adds a ban to the system.".to_string()),
        args: vec![FuncArg {
            name: Some("str".into()),
            r#type: Some(Type::String),
            ..Default::default()
        }],
        definition: Some(Definition::new_builtin(
            "ban".into(),
            Type::String, // Simplified, this is a function
            Some("Adds a ban to the system.".to_string()),
        )),
        ..Default::default()
    });

    let now = Type::Time;

    Definitions {
        properties: HashMap::from([
            ("req".into(),       Definition::new_builtin("req".into(),       req,       Some("These variables describe the present request".to_string()))),
            ("bereq".into(),     Definition::new_builtin("bereq".into(),     bereq,     Some("This is the request we send to the backend".to_string()))),
            ("req_top".into(),   Definition::new_builtin("req_top".into(),   req_top,   Some("When ESI:include requests are being processed, req_top points to the request received from the client.".to_string()))),
            ("resp".into(),      Definition::new_builtin("resp".into(),      resp,      Some("This is the response we send to the client".to_string()))),
            ("beresp".into(),    Definition::new_builtin("beresp".into(),    beresp,    Some("The response received from the backend".to_string()))),
            ("obj".into(),       Definition::new_builtin("obj".into(),       obj,       Some("This is the object we found in cache. It cannot be modified.".to_string()))),
            ("sess".into(),      Definition::new_builtin("sess".into(),      sess,      Some("A session corresponds to the \"conversation\" that Varnish has with a single client connection".to_string()))),
            ("client".into(),    Definition::new_builtin("client".into(),    client,    Some("These variables describe the network connection between the client and varnishd.".to_string()))),
            ("server".into(),    Definition::new_builtin("server".into(),    server,    Some("These variables describe the network connection on the Varnish server side.".to_string()))),
            ("local".into(),     Definition::new_builtin("local".into(),     local,     Some("Describes the local end of the TCP connection.".to_string()))),
            ("remote".into(),    Definition::new_builtin("remote".into(),    remote,    Some("Describes the remote end of the TCP connection (client or proxy).".to_string()))),
            ("regsub".into(),    Definition::new_builtin("regsub".into(),    regsub,    Some("Replaces the first occurrence of a regular expression in a string.".to_string()))),
            ("regsuball".into(), Definition::new_builtin("regsuball".into(), regsuball, Some("Replaces all occurrences of a regular expression in a string.".to_string()))),
            ("synthetic".into(), Definition::new_builtin("synthetic".into(), synthetic, Some("Generates a synthetic response.".to_string()))),
            ("hash_data".into(), Definition::new_builtin("hash_data".into(), hash_data, Some("Adds data to the hash for object lookup.".to_string()))),
            ("ban".into(),       Definition::new_builtin("ban".into(),       ban,       Some("Adds a ban to the system.".to_string()))),
            ("now".into(),       Definition::new_builtin("now".into(),       now,       Some("The current time.".to_string()))),
        ])
        .into_iter()
        .collect(),
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
