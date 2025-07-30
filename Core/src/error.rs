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
    pub error: [u8; 128],
}

impl ImagoError {
    pub const NULL: Self = Self {
        status: ImagoStatus::OK,
        error: [0; 128],
    };

    pub fn new<S>(status: ImagoStatus, message: S) -> Self
    where
        S: ToString,
    {
        let message = message.to_string();
        let mut error = [0u8; 128];

        unsafe {
            message
                .as_ptr()
                .copy_to_nonoverlapping(error.as_mut_ptr(), message.len().max(error.len()));
        }

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
