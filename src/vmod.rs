use goblin::elf::Elf;
use serde_json::{self, Value as SerdeValue};
use std::error::Error;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::path::PathBuf;

use crate::varnish_builtins::{Func, FuncArg, Obj, Type};

#[repr(C)]
#[derive(Debug)]
struct VmodDataCStruct {
    vrt_major: u32,
    vrt_minor: u32,
    file_id: *const c_char,
    name: *const c_char,
    #[cfg(feature = "varnish7")]
    func_name: *const c_char, // ADDED IN VARNISH 7
    func: *const c_char,
    func_len: i32,
    proto: *const c_char,
    json: *const c_char,
    abi: *const c_char,
}

#[derive(Debug)]
pub struct VmodData {
    pub vrt_major: usize,
    pub vrt_minor: usize,
    pub file_id: String,
    pub name: String,
    pub proto: String,
    pub json: String,
    pub abi: String,
    pub scope: Type,
}

fn parse_vmod_func_args(serde_value_arr: &[SerdeValue]) -> Vec<FuncArg> {
    serde_value_arr
        .iter()
        .filter_map(|arg| -> Option<_> {
            let arg_arr = arg.as_array()?;
            let input_type = arg_arr
                .first()?
                .as_str()
                .expect("Failed to find function argument type");
            let name = match arg_arr.get(1) {
                Some(SerdeValue::String(str)) => Some(str.to_string()),
                _ => None,
            };
            let default_value = match arg_arr.get(2) {
                Some(SerdeValue::String(str)) => Some(str.to_string()),
                _ => None,
            };
            let optional = match arg_arr.get(4) {
                Some(SerdeValue::Bool(bool)) => *bool,
                _ => false,
            };
            let r#type = match input_type {
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
                "BYTES" => Some(Type::String), // for now
                "BLOB" => Some(Type::Blob),
                "BACKEND" => Some(Type::Backend),
                "PROBE" => Some(Type::Probe),
                "ACL" => Some(Type::Acl),
                "HTTP" => Some(Type::Obj(Default::default())), // for now
                "HEADER" => Some(Type::String),
                "ENUM" => {
                    let enum_values = match arg_arr.get(3) {
                        Some(SerdeValue::Array(values)) => Some(
                            values
                                .iter()
                                .filter_map(|val| match val {
                                    SerdeValue::String(str) => Some(str.to_string()),
                                    _ => None,
                                })
                                .collect::<Vec<_>>(),
                        ),
                        _ => None,
                    }?;
                    Some(Type::Enum(enum_values))
                }
                _ => None,
            };

            // return None if no type
            r#type.as_ref()?;

            Some(FuncArg {
                name,
                default_value,
                optional,
                r#type,
            })
        })
        .collect::<Vec<_>>()
}

fn parse_vmod_json_func(
    serde_value_arr: &[SerdeValue],
) -> Result<Func, Box<dyn Error + Send + Sync>> {
    let name = serde_value_arr
        .get(1)
        .ok_or("Missing VMOD func name")?
        .as_str()
        .ok_or("VMOD func name is not string")?
        .to_string();

    let signature_arr = serde_value_arr
        .get(2)
        .ok_or("could not find method signature")?
        .as_array()
        .ok_or("method signature not array")?;

    let ret_types: Vec<String> = signature_arr
        .first()
        .ok_or("Missing return type field")?
        .as_array()
        .ok_or("Return type should be array")?
        .iter()
        .map(|ret_type| -> Result<String, Box<dyn Error>> {
            Ok(ret_type
                .as_str()
                .ok_or("Return type is not string")?
                .to_string())
        })
        .filter_map(|result| result.ok())
        .collect();

    let args = parse_vmod_func_args(&signature_arr[3..]);
    let ret_type = ret_types.first().ok_or("Missing return type")?.as_str();
    let r#return: Option<Box<Type>> = match ret_type {
        "BACKEND" => Some(Box::new(Type::Backend)),
        "STRING" => Some(Box::new(Type::String)),
        "REAL" => Some(Box::new(Type::Number)),
        "INT" => Some(Box::new(Type::Number)),
        "BOOL" => Some(Box::new(Type::Bool)),
        "VOID" => None,
        _ => None,
    };

    Ok(Func {
        name,
        args,
        ret_type: Some(ret_type.to_string()),
        r#return,
        ..Default::default()
    })
}

fn parse_vmod_json_obj(
    serde_value_arr: &[SerdeValue],
) -> Result<Func, Box<dyn Error + Send + Sync>> {
    let name = serde_value_arr
        .get(1)
        .ok_or("Failed to get obj name")?
        .as_str()
        .ok_or("Obj name is not string")?
        .to_string();

    let mut obj = Obj {
        name: name.clone(),
        read_only: true,
        ..Default::default()
    };

    for method_serde_val in serde_value_arr[6..].iter() {
        let method_arr = method_serde_val.as_array().ok_or("Method is not array")?;
        let func = parse_vmod_json_func(method_arr)?;
        obj.properties.insert(func.name.clone(), Type::Func(func));
    }

    let mut func = Func {
        name: name.clone(),
        ret_type: Some(name),
        r#return: Some(Box::new(Type::Obj(obj))),
        ..Default::default()
    };

    if let Some(SerdeValue::Array(ref vmod_init_def)) = serde_value_arr.get(4) {
        if let Some(SerdeValue::Array(ref array_containing_signature)) = vmod_init_def.get(1) {
            if let Some(signature_items) = array_containing_signature.get(4..) {
                func.args = parse_vmod_func_args(signature_items);
            }
        }
    }

    Ok(func)
}

pub fn parse_vmod_json(json: &str) -> Result<Type, Box<dyn Error + Send + Sync>> {
    let json_parsed: Vec<Vec<SerdeValue>> = serde_json::from_str(json)?;
    let mut vmod_obj = Obj {
        read_only: true,
        ..Default::default()
    };

    for row in json_parsed.iter() {
        let row_type = match row.first() {
            Some(SerdeValue::String(str)) => str.as_str(),
            _ => continue,
        };

        match row_type {
            "$VMOD" => {
                /*
                let value = row
                    .get(1)
                    .ok_or("Failed to parse VMOD version")?
                    .as_str()
                    .ok_or("VMOD version is not string")?
                    .to_string();
                vmod_json_data.vmod_version = value;
                */
            }
            "$EVENT" => {
                /*
                let name = row
                    .get(1)
                    .ok_or("Failed to get event name")?
                    .as_str_()
                    .ok_or("Event name is not string")?
                    .to_string();
                vmod_json_data.events.push(name);
                */
            }
            "$FUNC" => {
                if let Ok(func) = parse_vmod_json_func(row) {
                    vmod_obj
                        .properties
                        .insert(func.name.clone(), Type::Func(func));
                }
            }
            "$OBJ" => {
                if let Ok(func) = parse_vmod_json_obj(row) {
                    vmod_obj
                        .properties
                        .insert(func.name.clone(), Type::Func(func));
                }
            }
            _ => {}
        }
    }

    Ok(Type::Obj(vmod_obj))
}

pub async fn read_vmod_lib(
    vmod_name: String,
    path: PathBuf,
) -> Result<VmodData, Box<dyn Error + Send + Sync>> {
    let file = tokio::fs::read(path).await?;
    let elf = Elf::parse(&file)?;

    // Find symbol in symbol table
    let vmod_data_symbol_name = format!("Vmod_{}_Data", vmod_name);
    let vmd_sym = elf
        .dynsyms
        .iter()
        .find(|sym| {
            elf.dynstrtab
                .get_at(sym.st_name)
                .map(|sym_name| sym_name == vmod_data_symbol_name)
                .unwrap_or(false)
        })
        .ok_or("Could not find vmod data symbol")?;

    // Section for the symbol data
    let sec = &elf
        .section_headers
        .get(vmd_sym.st_shndx)
        .expect("Could not find section");

    // Offset in binary for symbol value
    let offset = sec.sh_offset as usize + vmd_sym.st_value as usize - sec.sh_addr as usize;
    // Transmute a pointer to the offset in the file, into a pointer to a VmodDataCStruct
    // let vmd = unsafe { &*std::mem::transmute::<*const u8, *const VmodDataCStruct>(&file[offset]) };
    #[allow(invalid_reference_casting)]
    let vmd = unsafe { &*std::mem::transmute::<*const u8, *const VmodDataCStruct>(&file[offset]) };

    let mut json: &str =
        &(unsafe { CStr::from_ptr(file[(vmd.json as usize)..].as_ptr() as *const c_char) }
            .to_string_lossy());

    if json.starts_with("VMOD_JSON_SPEC\u{2}") {
        json = &(json[(json.find('\u{2}').unwrap() + 1)..json.find('\u{3}').unwrap()]);
    }

    let vmod_json_data = parse_vmod_json(json)?;
    return Ok(VmodData {
        vrt_major: vmd.vrt_major as usize,
        vrt_minor: vmd.vrt_minor as usize,
        name: unsafe { CStr::from_ptr((file[(vmd.name as usize)..].as_ptr()) as *const c_char) }
            .to_string_lossy()
            .to_string(),
        file_id: unsafe {
            CStr::from_ptr((file[(vmd.file_id as usize)..].as_ptr()) as *const c_char)
        }
        .to_string_lossy()
        .to_string(),
        proto: unsafe { CStr::from_ptr((file[(vmd.proto as usize)..].as_ptr()) as *const c_char) }
            .to_string_lossy()
            .to_string(),
        abi: unsafe { CStr::from_ptr((file[(vmd.abi as usize)..].as_ptr()) as *const c_char) }
            .to_string_lossy()
            .to_string(),
        json: json.to_string(),
        scope: vmod_json_data,
    });
}

pub async fn read_vmod_lib_by_name(
    name: String,
    search_paths: Vec<PathBuf>,
) -> Result<Option<VmodData>, Box<dyn Error + Send + Sync>> {
    let file_name = format!("libvmod_{}.so", name);
    for search_path in search_paths {
        let path = search_path.join(&file_name);
        if path.exists() {
            return Ok(Some(read_vmod_lib(name, path).await?));
        }
    }

    Ok(None)
}
