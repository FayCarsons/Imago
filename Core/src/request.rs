use crate::error::{ImagoError, ImagoStatus};
use crate::formats::Format;
use crate::operations::Operation;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::slice;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ByteArray {
    pub len: usize,
    pub data: *const u8,
}

impl ByteArray {
    /// # Safety
    pub unsafe fn new(data: *const u8, len: usize) -> Self {
        Self { data, len }
    }

    // NOTE:(@faycarsons)
    // Write "FAILURE" to the ByteArray, with the hope that if everything else fails
    // users will at least get a message like "Error: 'FAILURE' is not a valid image" to
    // unambiguously let them know something has gone wrong
    pub fn failure_string() -> *mut Self {
        let s = "FAILURE";

        let this = Box::new(ByteArray {
            len: s.len(),
            data: s.as_ptr(),
        });

        Box::into_raw(this)
    }

    pub fn to_string_lossy(&self) -> String {
        if self.data.is_null() || self.len == 0 {
            return String::new();
        }

        let slice = unsafe { slice::from_raw_parts(self.data, self.len) };

        String::from_utf8_lossy(slice).to_string()
    }
}

impl From<Vec<u8>> for ByteArray {
    fn from(value: Vec<u8>) -> Self {
        let (data, len, _) = value.into_raw_parts();
        ByteArray { data, len }
    }
}

// TODO: (@faycarsons)
// Off the top of my head, I can't think of a better way to do this besides
// effectively cloning and then leaking
// But may be worth looking into to reduce allocations?
impl From<&[u8]> for ByteArray {
    fn from(value: &[u8]) -> Self {
        Self::from(value.to_vec())
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
            .map(Self)
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
        input_format: Option<Format>,
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
            Some(fmt) => Ok(Some(fmt)),
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

#[repr(C)]
pub struct Request<'a, A: Arguments> {
    pub args: A,
    pub pipeline: &'a [Operation],
}

impl<'a> Request<'a, FileArgs<'a>> {
    pub fn build_file(
        input_path: *const c_char,
        operations_data: *const Operation,
        operations_len: usize,
    ) -> Result<Self, ImagoError> {
        let args = FileArgs::decode(input_path)
            .ok_or_else(|| ImagoError::new(ImagoStatus::InvalidPath, "Invalid file path"))?;
        let pipeline = decode_pipeline(operations_data, operations_len).ok_or_else(|| {
            ImagoError::new(
                ImagoStatus::InvalidOperation,
                "Internal error: operation pipline null or otherwise invalid",
            )
        })?;

        Ok(Self { args, pipeline })
    }
}

impl<'a> Request<'a, BufferArgs<'a>> {
    pub fn build_buffer(
        buffer_data: *const u8,
        buffer_len: usize,
        input_format: Option<Format>,
        operations_data: *const Operation,
        operations_len: usize,
    ) -> Result<Self, ImagoError> {
        let args = BufferArgs::decode(buffer_data, buffer_len, input_format)?;
        let pipeline = decode_pipeline(operations_data, operations_len).ok_or_else(|| {
            ImagoError::new(
                ImagoStatus::InvalidOperation,
                "Internal error, null or otherwise invalid operation pipeline",
            )
        })?;

        Ok(Self { args, pipeline })
    }
}
