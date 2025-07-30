use crate::ByteArray;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum ImagoStatus {
    OK,
    InvalidPath,
    InvalidInputBuffer,
    InvalidOperation,
    InvalidInputFormat,
    InvalidOutputFormat,
    UnsupportedImageFormat,
    LoadFailed,
    EncodeFailed,
    NullContext,
}

pub struct ImagoError {
    pub status: ImagoStatus,
    pub error: ByteArray,
}

impl ImagoError {
    pub const NULL: Self = Self {
        status: ImagoStatus::OK,
        error: ByteArray::NULL,
    };

    pub fn new<S>(status: ImagoStatus, message: S) -> Self
    where
        S: ToString,
    {
        let message = message.to_string();

        let (data, len, _) = message.into_raw_parts();
        let error = unsafe { ByteArray::new(data, len) };
        Self { status, error }
    }
}

pub struct Context(pub ImagoError);

impl Context {
    pub fn success() -> Self {
        Self(ImagoError::NULL)
    }

    pub fn failure<S>(status: ImagoStatus, message: S) -> Self
    where
        S: ToString,
    {
        Self(ImagoError::new(status, message))
    }

    pub fn from_error(e: ImagoError) -> Self {
        Self(e)
    }
}
