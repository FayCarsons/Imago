#![feature(vec_into_raw_parts)]

mod error;
mod ffi;
mod formats;
mod info;
mod operations;
mod request;

// Re-export public API
pub use error::{Context, ImagoError, ImagoStatus};
pub use ffi::*;
pub use formats::{ColorType, Format, OptionalFormat};
pub use info::ImageInfo;
pub use operations::{Degree, Direction, FilterType, Operation};
pub use request::ByteArray;
