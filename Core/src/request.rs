use std::ffi::CStr;
use std::os::raw::c_char;
use std::slice;
use crate::error::{ImagoError, ImagoStatus};
use crate::formats::Format;
use crate::operations::Operation;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ByteArray {
    pub len: usize,
    pub data: *const u8,
}

impl ByteArray {
    pub fn failure_string() -> *mut Self {
        let s = "Failure";

        let this = Box::new(ByteArray {
            len: s.len(),
            data: s.as_ptr(),
        });

        Box::into_raw(this)
    }
}

impl From<Vec<u8>> for ByteArray {
    fn from(value: Vec<u8>) -> Self {
        let (data, len, _) = value.into_raw_parts();
        ByteArray { data, len }
    }
}

pub trait Arguments {}

pub struct FileArgs<'a>(pub &'a str);

impl<'a> Arguments for FileArgs<'a> {}

impl<'a> FileArgs<'a> {
    pub fn decode(input_path: *const c_char) -> Option<Self> {
        (!input_path.is_null())
            .then(|| unsafe { CStr::from_ptr(input_path) })
            .and_then(|s| s.to_str().ok())
            .map(|input_path| Self(input_path))
    }
}

pub struct BufferArgs<'a> {
    pub input_buffer: &'a [u8],
    pub input_format: Option<Format>,
}

impl<'a> Arguments for BufferArgs<'a> {}

impl<'a> BufferArgs<'a> {
    pub fn decode(
        buffer_data: *const u8,
        buffer_len: usize,
        input_format: Option<*const Format>,
    ) -> Result<Self, ImagoError> {
        let input_buffer = (!buffer_data.is_null())
            .then(|| unsafe { slice::from_raw_parts(buffer_data, buffer_len) })
            .ok_or_else(|| {
                ImagoError::new(
                    ImagoStatus::InvalidInputBuffer,
                    "Internal error, null or otherwise invalid operation pipeline",
                )
            })?;

        let input_format = match input_format {
            Some(fmt) => {
                if fmt.is_null() {
                    Err(ImagoError::new(
                        ImagoStatus::InvalidInputFormat,
                        "Internal error, nulll or otherwise invalid input format",
                    ))
                } else {
                    unsafe { Ok(Some(fmt.read())) }
                }
            }
            None => Ok(None),
        }?;

        Ok(Self {
            input_buffer,
            input_format,
        })
    }
}

pub fn decode_pipeline<'a>(
    operations_data: *const Operation,
    operations_len: usize,
) -> Option<&'a [Operation]> {
    (!operations_data.is_null())
        .then(|| unsafe { slice::from_raw_parts(operations_data, operations_len) })
}

pub fn decode_output_format(fmt: *const Format) -> Option<Format> {
    (!fmt.is_null()).then(|| unsafe { fmt.read() })
}

#[repr(C)]
pub struct Request<'a, A: Arguments> {
    pub args: A,
    pub pipeline: &'a [Operation],
    pub output_format: Format,
}

impl<'a> Request<'a, FileArgs<'a>> {
    pub fn build_file(
        input_path: *const c_char,
        operations_data: *const Operation,
        operations_len: usize,
        output_format: *const Format,
    ) -> Result<Self, ImagoError> {
        let args = FileArgs::decode(input_path)
            .ok_or_else(|| ImagoError::new(ImagoStatus::InvalidPath, "Invalid file path"))?;
        let pipeline = decode_pipeline(operations_data, operations_len).ok_or_else(|| {
            ImagoError::new(
                ImagoStatus::InvalidOperation,
                "Internal error: operation pipline null or otherwise invalid",
            )
        })?;
        let output_format = decode_output_format(output_format).ok_or_else(|| {
            ImagoError::new(
                ImagoStatus::InvalidOutputFormat,
                "Internal error, null or otherwise invalid output format",
            )
        })?;

        Ok(Self {
            args,
            pipeline,
            output_format,
        })
    }
}

impl<'a> Request<'a, BufferArgs<'a>> {
    pub fn build_buffer(
        buffer_data: *const u8,
        buffer_len: usize,
        input_format: Option<*const Format>,
        operations_data: *const Operation,
        operations_len: usize,
        output_format: *const Format,
    ) -> Result<Self, ImagoError> {
        let args = BufferArgs::decode(buffer_data, buffer_len, input_format)?;
        let pipeline = decode_pipeline(operations_data, operations_len).ok_or_else(|| {
            ImagoError::new(
                ImagoStatus::InvalidOperation,
                "Internal error, null or otherwise invalid operation pipeline",
            )
        })?;
        let output_format = decode_output_format(output_format).ok_or_else(|| {
            ImagoError::new(
                ImagoStatus::InvalidOutputFormat,
                "Internal error, null output format",
            )
        })?;

        Ok(Self {
            args,
            pipeline,
            output_format,
        })
    }
}