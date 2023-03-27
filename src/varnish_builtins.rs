use std::collections::HashMap;

#[derive(Debug)]
pub enum Type {
    Obj(Obj),
    Func(Func),
    Backend,
    String,
    Number,
    Duration,
    Bool,
}

#[derive(Debug)]
pub struct Obj {
    pub name: String,
    pub properties: HashMap<String, Type>,
    pub read_only: bool,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
}

// const globals: HashMap<str, Type> = HashMap::from([req, resp, obj, client].map(|global_var| match ));

pub fn get_varnish_builtins() -> Type {
    let req: Type = Type::Obj(Obj {
        name: "req".to_string(),
        read_only: true,
        properties: HashMap::from([
            (
                "http".to_string(),
                Type::Obj(Obj {
                    name: "req.http".to_string(),
                    read_only: false,
                    properties: HashMap::from([
                        ("host".to_string(), Type::String),
                        ("origin".to_string(), Type::String),
                        ("cookie".to_string(), Type::String),
                        ("user-agent".to_string(), Type::String),
                        ("referer".to_string(), Type::String),
                        ("if-none-match".to_string(), Type::String),
                        ("if-modified-since".to_string(), Type::String),
                        ("accept".to_string(), Type::String),
                    ]),
                }),
            ),
            ("url".to_string(), Type::String),
            ("method".to_string(), Type::Backend),
            ("hash".to_string(), Type::Backend),
            ("proto".to_string(), Type::Backend),
            ("backend_hint".to_string(), Type::Backend),
            ("restarts".to_string(), Type::Number),
            ("ttl".to_string(), Type::Duration),
            ("grace".to_string(), Type::Duration),
            ("is_hitmiss".to_string(), Type::Bool),
            ("is_hitpass".to_string(), Type::Bool),
        ]),
    });

    let resp: Type = Type::Obj(Obj {
        name: "resp".to_string(),
        read_only: true,
        properties: HashMap::from([
            (
                "http".to_string(),
                Type::Obj(Obj {
                    name: "req.http".to_string(),
                    read_only: false,
                    properties: HashMap::from([
                        ("vary".to_string(), Type::String),
                        ("origin".to_string(), Type::String),
                        ("server".to_string(), Type::String),
                        ("age".to_string(), Type::String),
                        ("expires".to_string(), Type::String),
                        ("etag".to_string(), Type::String),
                        ("last-modified".to_string(), Type::String),
                        ("content-type".to_string(), Type::String),
                        ("cache-control".to_string(), Type::String),
                        ("surrogate-control".to_string(), Type::String),
                    ]),
                }),
            ),
            ("status".to_string(), Type::Number),
            ("reason".to_string(), Type::String),
            ("backend".to_string(), Type::Backend),
            ("is_streaming".to_string(), Type::Bool),
        ]),
    });

    let obj: Type = Type::Obj(Obj {
        name: "obj".to_string(),
        read_only: true,
        properties: HashMap::from([
            ("ttl".to_string(), Type::Duration),
            ("grace".to_string(), Type::Duration),
            ("keep".to_string(), Type::Duration),
            ("age".to_string(), Type::Duration),
            ("hits".to_string(), Type::Number),
            ("uncacheable".to_string(), Type::Bool),
        ]),
    });

    let client: Type = Type::Obj(Obj {
        name: "client".to_string(),
        read_only: true,
        properties: HashMap::from([
            ("ip".to_string(), Type::String),
            ("identity".to_string(), Type::String),
        ]),
    });

    let server: Type = Type::Obj(Obj {
        name: "client".to_string(),
        read_only: true,
        properties: HashMap::from([
            ("ip".to_string(), Type::String),
            ("hostname".to_string(), Type::String),
            ("identity".to_string(), Type::String),
        ]),
    });

    let regsub = Type::Func(Func {
        name: "regsub".to_string(),
    });

    let regsuball = Type::Func(Func {
        name: "regsuball".to_string(),
    });

    let global_scope: Type = Type::Obj(Obj {
        name: "GLOBAL".to_string(),
        read_only: true,
        properties: HashMap::from([
            ("req".to_string(), req),
            // ("bereq".to_string(), req), // TODO: FIXME:
            ("resp".to_string(), resp),
            // ("beresp".to_string(), resp), // TODO: FIXME:
            ("obj".to_string(), obj),
            ("client".to_string(), client),
            ("server".to_string(), server),

            ("regsub".to_string(), regsub),
            ("regsuball".to_string(), regsuball),
        ]),
    });

    return global_scope;
}
