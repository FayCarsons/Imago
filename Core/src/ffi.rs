use crate::error::{Context, ImagoStatus};
use crate::formats::{Format, decode_image, encode_image};
use crate::info::{ImageInfo, get_buffer_info_inner, get_image_info_inner};
use crate::operations::{Interpreter, Operation, apply_operation};
use crate::request::{ByteArray, Request};
use image;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::slice;

#[unsafe(no_mangle)]
pub extern "C" fn get_image_info(ctx: *mut Context, input_path: *const c_char) -> *mut ImageInfo {
    if ctx.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::NullContext, "Context is null");
            return std::ptr::null_mut();
        }
    }

    if input_path.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::InvalidPath, "Input path null");
            return std::ptr::null_mut();
        }
    }

    let path = unsafe {
        match CStr::from_ptr(input_path).to_str() {
            Ok(path) => path,
            Err(e) => {
                dbg!(e);
                let err = format!("Internal error: {e}");
                *ctx = Context::failure(ImagoStatus::InvalidPath, err);
                return std::ptr::null_mut();
            }
        }
    };

    match get_image_info_inner(path) {
        Ok(info) => unsafe {
            *ctx = Context::success();
            let boxed = Box::new(info);
            Box::into_raw(boxed)
        },
        Err(err) => unsafe {
            *ctx = Context::from_error(err);
            std::ptr::null_mut()
        },
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn get_buffer_info(
    ctx: *mut Context,
    buffer_data: *const u8,
    buffer_len: usize,
) -> *mut ImageInfo {
    if ctx.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::NullContext, "Context is null");
            return std::ptr::null_mut();
        }
    }

    if buffer_data.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::InvalidInputBuffer, "Buffer data is null");
            return std::ptr::null_mut();
        }
    }

    let buffer = unsafe { slice::from_raw_parts(buffer_data, buffer_len) };

    match get_buffer_info_inner(buffer) {
        Ok(info) => unsafe {
            *ctx = Context::success();
            let boxed = Box::new(info);
            Box::into_raw(boxed)
        },
        Err(err) => unsafe {
            *ctx = Context::from_error(err);
            std::ptr::null_mut()
        },
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn destroy_image_info(image_info: *mut ImageInfo) {
    if !image_info.is_null() {
        unsafe {
            let _ = Box::from_raw(image_info);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn make_context() -> *mut Context {
    let context = Box::new(Context::success());
    Box::into_raw(context)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn destroy_context(ctx: *mut Context) {
    if !ctx.is_null() {
        unsafe {
            let _ = Box::from_raw(ctx);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn get_status(ctx: *mut Context) -> ImagoStatus {
    unsafe { ctx.read().0.status }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn get_error_message(ctx: *mut Context) -> *mut c_char {
    if ctx.is_null() {
        return std::ptr::null_mut();
    }
    
    let context = unsafe { &*ctx };
    match context.0.status {
        ImagoStatus::OK => std::ptr::null_mut(),
        _ => {
            let error_string = context.0.error.to_string_lossy();
            let c_string = CString::new(error_string).unwrap_or_default();
            c_string.into_raw()
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn destroy_error_message(msg: *mut c_char) {
    if !msg.is_null() {
        unsafe { 
            let _ = CString::from_raw(msg); 
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_image(
    ctx: *mut Context,
    input_path: *const c_char,
    operations_data: *const Operation,
    operations_len: usize,
) -> *mut ByteArray {
    let request = match Request::build_file(input_path, operations_data, operations_len) {
        Ok(req) => req,
        Err(status) => unsafe {
            *ctx = Context::from_error(status);
            return ByteArray::failure_string();
        },
    };

    let img = match image::open(request.args.0) {
        Ok(img) => img,
        Err(e) => unsafe {
            *ctx = Context::failure(ImagoStatus::LoadFailed, e);
            return ByteArray::failure_string();
        },
    };

    let interpreter = request
        .pipeline
        .iter()
        .fold(Interpreter::new(img), apply_operation);

    let buf = match encode_image(interpreter) {
        Ok(raw) => raw,
        Err(status) => unsafe {
            *ctx = Context::from_error(status);
            return ByteArray::failure_string();
        },
    };

    let bytes = ByteArray::from(buf).into();
    Box::into_raw(bytes)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_buffer(
    ctx: *mut Context,
    content: *mut u8,
    content_len: usize,
    input_format: *const Format,
    operations_data: *const Operation,
    operations_len: usize,
) -> *mut ByteArray {
    if ctx.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::NullContext, "Internal error, context null");
            return ByteArray::failure_string();
        }
    }

    let input_format = (!input_format.is_null()).then(|| input_format);

    let request = match Request::build_buffer(
        content,
        content_len,
        input_format,
        operations_data,
        operations_len,
    ) {
        Ok(request) => request,
        Err(e) => unsafe {
            *ctx = Context::from_error(e);
            return ByteArray::failure_string();
        },
    };

    let img = match decode_image(request.args.input_buffer, request.args.input_format) {
        Ok(img) => img,
        Err(e) => unsafe {
            *ctx = Context::from_error(e);
            return ByteArray::failure_string();
        },
    };

    match image::load_from_memory(request.args.input_buffer) {
        Ok(img) => img,
        Err(e) => unsafe {
            (*ctx) = Context::failure(ImagoStatus::LoadFailed, e);
            return ByteArray::failure_string();
        },
    };

    let interpreter = request
        .pipeline
        .iter()
        .fold(Interpreter::new(img), apply_operation);

    let buf = match encode_image(interpreter) {
        Ok(raw) => ByteArray::from(raw).into(),
        Err(e) => unsafe {
            *ctx = Context::from_error(e);
            return ByteArray::failure_string();
        },
    };

    unsafe { *ctx = Context::success() }
    Box::into_raw(buf)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn destroy_output_buffer(bytes: *mut ByteArray) {
    if !bytes.is_null() {
        unsafe {
            let _ = Box::from_raw(bytes);
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_operations(
    operations_data: *const Operation,
    operations_len: usize,
) {
    unsafe {
        if !operations_data.is_null() {
            let ops = slice::from_raw_parts(operations_data, operations_len);
            println!("{:#?}", ops)
        }
    }
}
