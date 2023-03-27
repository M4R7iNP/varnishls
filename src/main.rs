#![feature(pointer_byte_offsets)]

use goblin::elf::Elf;
use serde::{Deserialize, Serialize};
use serde_json::{self, Value as SerdeValue};
use std::error::Error;
use std::ffi::CStr;
use std::os::raw::c_char;
use vcl_parser::parser;

#[repr(C)]
#[derive(Debug)]
struct VmodData {
    vrt_major: u32,
    vrt_minor: u32,
    file_id: *const c_char,
    name: *const c_char,
    func: *const u8,
    func_len: i32,
    proto: *const c_char,
    json: *const c_char,
    abi: *const c_char,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum VmodFuncValue {
    String(String),
    Int(i32),
    Real(f32),
    Bool(bool),
    Bytes(String),
    Enum(String),
    Void,
    StringList(Vec<String>),
    Header(String),
    Duration(String),
    Ip(String),
    Backend(String),
    PrivCall,
    Http(String),
}

#[derive(Debug)]
struct VmodFuncArg {
    name: String,
    input_type: String,
}

#[derive(Debug)]
struct VmodFunc {
    name: String,
    args: Vec<VmodFuncArg>,
    ret_type: String,
}

#[derive(Debug)]
struct VmodJsonData {
    vmod_version: String,
    events: Vec<String>,
    funcs: Vec<VmodFunc>,
}

fn parse_vmod_json_func(serde_value_arr: &Vec<SerdeValue>) -> Result<VmodFunc, Box<dyn Error>> {
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
        .get(0)
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
        .filter(|result| result.is_ok())
        .map(|result| result.unwrap())
        .collect();

    let args = signature_arr[3..signature_arr.len()]
        .iter()
        .map(|arg| -> Result<VmodFuncArg, Box<dyn Error>> {
            let arg_arr = arg.as_array().ok_or("Arg signature is not array")?;
            Ok(VmodFuncArg {
                input_type: arg_arr
                    .get(0)
                    .ok_or("Missing VMOD method arg type")?
                    .as_str()
                    .ok_or("VMOD method arg type should be string")?
                    .to_string(),
                name: arg_arr
                    .get(1)
                    .ok_or("Missing VMOD method arg name")?
                    .as_str()
                    .ok_or("VMOD method arg name should be string")?
                    .to_string(),
            })
        })
        .filter(|result| result.is_ok())
        .map(|result| result.unwrap())
        .collect();

    Ok(VmodFunc {
        name,
        args,
        ret_type: ret_types.get(0).ok_or("Missing return type")?.to_string(),
    })
}

fn parse_vmod_json(json: &str) -> Result<VmodJsonData, Box<dyn Error>> {
    let json_parsed: Vec<Vec<SerdeValue>> = serde_json::from_str(&json)?;
    // println!("json test: {:?}", json_parsed);
    let mut vmod_json_data = VmodJsonData {
        vmod_version: String::new(),
        events: Vec::new(),
        funcs: Vec::new(),
    };

    for row in json_parsed.iter() {
        let row_type = row.get(0).ok_or("empty array")?.as_str();
        if row_type.is_none() {
            continue;
        }

        let row_type = row_type.unwrap();

        match row_type {
            "$VMOD" => {
                let value = row
                    .get(1)
                    .ok_or("Failed to parse VMOD version")?
                    .as_str()
                    .ok_or("VMOD version is not string")?
                    .to_string();
                vmod_json_data.vmod_version = value;
            }
            "$EVENT" => {
                let name = row
                    .get(1)
                    .ok_or("Failed to get event name")?
                    .as_str()
                    .ok_or("Event name is not string")?
                    .to_string();
                vmod_json_data.events.push(name);
            }
            "$FUNC" => {
                let func = parse_vmod_json_func(&row)?;
                vmod_json_data.funcs.push(func);
            }
            "$OBJ" => {
                // TODO: implement
            }
            _ => {}
        }
    }

    return Ok(vmod_json_data);
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();
    match args.get(1) {
        Some(str) if str == "--vcl" => {
            println!("Parsing vcl…");
            let file = std::fs::read_to_string(args.get(2).ok_or("Missing vcl path")?)?;
            let result = parser::parse(&file);
            println!("{:?}", result);
            return Ok(());
        }
        _ => {}
    }

    let vmod_name = args.get(1).expect("Missing vmod name");
    let lib_handle = format!("/usr/lib/varnish-plus/vmods/libvmod_{}.so", vmod_name);
    let file = std::fs::read(lib_handle)?;
    let elf = Elf::parse(&file)?;

    let vmod_data_symbol_name = format!("Vmod_{}_Data", vmod_name);
    let (vmd_sym_idx, vmd_sym) = elf
        .dynsyms
        .iter()
        .enumerate()
        .find(|(_, sym)| {
            elf.dynstrtab
                .get_at(sym.st_name)
                .and_then(|sym_name| Some(sym_name == vmod_data_symbol_name))
                .unwrap_or_else(|| false)
        })
        .ok_or("Could not find vmod data symbol")?;

    let sec = &elf
        .section_headers
        .get(vmd_sym.st_shndx)
        .expect("Could not find section");
    let offset = sec.sh_offset as usize + vmd_sym_idx as usize * sec.sh_entsize as usize;
    let vmd_ptr = unsafe { file.as_ptr().byte_offset(offset as isize) as *const VmodData };

    let vmd = unsafe { &*vmd_ptr };
    let file_ptr = file.as_ptr();
    println!("vmd: {:?}", vmd);
    println!("major: {}", vmd.vrt_major);
    println!("minor: {}", vmd.vrt_minor);
    println!("name: {}", unsafe {
        CStr::from_ptr(file_ptr.byte_offset(vmd.name as isize) as *const i8).to_string_lossy()
    });
    println!("file_id: {}", unsafe {
        CStr::from_ptr(file_ptr.byte_offset(vmd.file_id as isize) as *const i8).to_string_lossy()
    });
    println!("func_len: {}", vmd.func_len);
    println!("proto: {}", unsafe {
        CStr::from_ptr(file_ptr.byte_offset(vmd.proto as isize) as *const i8).to_string_lossy()
    });
    println!("abi: {}", unsafe {
        CStr::from_ptr(file_ptr.byte_offset(vmd.abi as isize) as *const i8).to_string_lossy()
    });
    let json = unsafe {
        CStr::from_ptr(file_ptr.byte_offset(vmd.json as isize) as *const i8).to_string_lossy()
    };
    println!("json: {}", json);

    let vmod_json_data = parse_vmod_json(&json).unwrap();
    println!("VMOD JSON DATA: {:?}", vmod_json_data);

    for func in vmod_json_data.funcs.iter() {
        print!("{}.{}(", vmod_name, func.name);

        let arg_len = func.args.len();
        for i in 0..arg_len {
            let arg = &func.args[i];
            print!("{}: {}", arg.name, arg.input_type);
            if i < arg_len - 1 {
                print!(", ");
            }
        }

        print!(")\n");
    }

    return Ok(());
}
