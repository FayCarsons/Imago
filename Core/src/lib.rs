#![feature(vec_into_raw_parts)]
use image::imageops::FilterType as ImageFilter;
use image::{DynamicImage, ImageFormat};
use std::ffi::CStr;
use std::io::Cursor;
use std::os::raw::c_char;
use std::path::PathBuf;
use std::slice;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum ImagoStatus {
    OK = 0,
    InvalidPath = 1,
    InvalidOperation = 2,
    InvalidOutputFormat = 3,
    LoadFailed = 4,
    EncodeFailed = 5,
    NullContext = 6,
}

impl ImagoStatus {
    fn message(self) -> &'static str {
        match self {
            ImagoStatus::OK => "Success",
            ImagoStatus::InvalidPath => "Invalid image path",
            ImagoStatus::InvalidOperation => "Invalid operations data",
            ImagoStatus::InvalidOutputFormat => "Invalid output format",
            ImagoStatus::LoadFailed => "Loading image failed",
            ImagoStatus::EncodeFailed => "Failed to encode image",
            ImagoStatus::NullContext => "Null or otherwise invalid context",
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ByteArray {
    pub data: *const u8,
    pub len: usize,
}

impl From<Vec<u8>> for ByteArray {
    fn from(value: Vec<u8>) -> Self {
        let (data, len, _) = value.into_raw_parts();
        ByteArray { data, len }
    }
}

impl From<String> for ByteArray {
    fn from(value: String) -> Self {
        let (data, len, _) = String::into_raw_parts(value);
        ByteArray { data, len }
    }
}

impl From<&'static str> for ByteArray {
    fn from(value: &'static str) -> Self {
        let bytes = value.as_bytes();
        let data = bytes.as_ptr();
        let len = bytes.len();

        Self { data, len }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
struct Request {
    pipeline: Vec<Operation>,
    input_path: PathBuf,
    output_format: OutputFormat,
}

impl Request {
    fn build(
        input_path: *const c_char,
        operations_data: *const Operation,
        operations_len: usize,
        output_format: *const OutputFormat,
    ) -> Result<Self, ImagoStatus> {
        unsafe {
            if input_path.is_null() {
                return Err(ImagoStatus::InvalidPath);
            }

            if operations_data.is_null() {
                return Err(ImagoStatus::InvalidOperation);
            }

            if output_format.is_null() {
                return Err(ImagoStatus::InvalidOutputFormat);
            }

            let input_path = PathBuf::from(
                CStr::from_ptr(input_path)
                    .to_str()
                    .map_err(|_| ImagoStatus::InvalidPath)?,
            );

            let pipeline = slice::from_raw_parts(operations_data, operations_len).to_owned();

            let output_format = output_format.read();

            Ok(Request {
                pipeline,
                input_path,
                output_format,
            })
        }
    }
}

#[repr(C, u8)]
#[derive(Debug, Clone)]
pub enum OutputFormat {
    WebP = 0,
    Png = 1,
    Jpeg(u8) = 2,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum FilterType {
    Nearest = 0,
    Triangle = 1,
    CatmullRom = 2,
    Gaussian = 3,
    Lanczos3 = 4,
}

impl Into<ImageFilter> for FilterType {
    fn into(self) -> ImageFilter {
        match self {
            Self::Nearest => ImageFilter::Nearest,
            Self::Triangle => ImageFilter::Triangle,
            Self::CatmullRom => ImageFilter::CatmullRom,
            Self::Gaussian => ImageFilter::Gaussian,
            Self::Lanczos3 => ImageFilter::Lanczos3,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub enum Direction {
    Horizontal = 0,
    Vertical = 1,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub enum Degree {
    Deg90 = 0,
    Deg180 = 1,
    Deg270 = 2,
}

#[repr(C, u8)]
#[derive(Debug, Clone)]
pub enum Operation {
    Resize {
        w: u32,
        h: u32,
        filter: FilterType,
        exact: bool,
    },
    Rotate(Degree),
    Flip(Direction),
    GrayScale,
    Blur(f32),
    Brighten(i32),
    Contrast(f32),
}

fn apply_operation(img: DynamicImage, operation: &Operation) -> DynamicImage {
    match operation {
        Operation::Resize {
            w,
            h,
            filter,
            exact,
        } => {
            if *exact {
                img.resize_exact(*w, *h, (*filter).into())
            } else {
                img.resize(*w, *h, (*filter).into())
            }
        }
        Operation::Rotate(Degree::Deg90) => img.rotate90(),
        Operation::Rotate(Degree::Deg180) => img.rotate180(),
        Operation::Rotate(Degree::Deg270) => img.rotate270(),
        Operation::Flip(Direction::Horizontal) => img.fliph(),
        Operation::Flip(Direction::Vertical) => img.flipv(),
        Operation::GrayScale => DynamicImage::ImageLuma8(img.to_luma8()),
        Operation::Blur(radius) => img.blur(*radius),
        Operation::Brighten(delta) => img.brighten(*delta),
        Operation::Contrast(factor) => img.adjust_contrast(*factor),
    }
}

fn encode_image(img: &DynamicImage, format: &OutputFormat) -> Result<Vec<u8>, ImagoStatus> {
    let mut output = Vec::new();
    let mut cursor = Cursor::new(&mut output);

    match format {
        OutputFormat::WebP => img.write_to(&mut cursor, ImageFormat::WebP),
        OutputFormat::Jpeg(quality) => {
            let mut encoder = image::codecs::jpeg::JpegEncoder::new_with_quality(cursor, *quality);
            encoder.encode_image(img)
        }
        OutputFormat::Png => img.write_to(&mut cursor, ImageFormat::Png),
    }
    .map_err(|_| ImagoStatus::EncodeFailed)?;

    Ok(output)
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Response {
    bytes: *mut u8,
    len: usize,
    status: ImagoStatus,
}

impl Response {
    fn failure(status: ImagoStatus) -> Self {
        Response {
            bytes: std::ptr::null_mut(),
            len: 0,
            status,
        }
    }

    fn success(bytes: Vec<u8>) -> Self {
        let (bytes, len, _) = bytes.into_raw_parts();

        Self {
            bytes,
            len,
            status: ImagoStatus::OK,
        }
    }
}

pub struct Context {
    status: ImagoStatus,
    output_len: usize,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn make_context() -> *mut Context {
    let context = Context {
        status: ImagoStatus::OK,
        output_len: 0,
    }
    .into();

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
    unsafe { ctx.read().status }
}

#[unsafe(no_mangle)]
pub extern "C" fn get_output_len(ctx: *mut Context) -> usize {
    unsafe { ctx.read().output_len }
}

// In a perfect world this would be a function of Path -> [Operation] -> OutputFormat -> IO ()
// But instead we have to unmarshal all our values from haskell
// Ideally, we are relying on the C ABI *as much as possible*
// Manual encoding/decoding or relying on language/library quirks is not acceptable
#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_image(
    ctx: *mut Context,
    input_path: *const c_char,
    operations_data: *const Operation,
    operations_len: usize,
    output_format: *const OutputFormat,
) -> *mut u8 {
    let request = match Request::build(input_path, operations_data, operations_len, output_format) {
        Ok(req) => req,
        Err(status) => unsafe {
            *ctx = Context {
                status,
                output_len: 0,
            };
            return std::ptr::null_mut();
        },
    };

    let img = match image::open(request.input_path) {
        Ok(img) => img,
        Err(_) => unsafe {
            *ctx = Context {
                status: ImagoStatus::LoadFailed,
                output_len: 0,
            };
            return std::ptr::null_mut();
        },
    };

    let transformed = request.pipeline.iter().fold(img, apply_operation);

    let (buf, buflen, _) = match encode_image(&transformed, &request.output_format) {
        Ok(raw) => raw.into_raw_parts(),
        Err(status) => unsafe {
            *ctx = Context {
                status,
                output_len: 0,
            };
            return std::ptr::null_mut();
        },
    };

    unsafe { (*ctx).output_len = buflen }
    buf
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_buffer(
    ctx: *mut Context,
    content: *mut u8,
    content_len: usize,
    operations_data: *const Operation,
    operations_len: usize,
    output_format: *const OutputFormat,
) -> *mut u8 {
    if ctx.is_null() {
        unsafe {
            *ctx = Context {
                status: ImagoStatus::NullContext,
                output_len: 0,
            };

            return std::ptr::null_mut();
        }
    }

    if content.is_null() {
        unsafe {
            (*ctx).status = ImagoStatus::LoadFailed;
            return std::ptr::null_mut();
        }
    }

    if operations_data.is_null() {
        unsafe {
            (*ctx).status = ImagoStatus::InvalidOperation;
            return std::ptr::null_mut();
        }
    }

    if output_format.is_null() {
        unsafe {
            (*ctx).status = ImagoStatus::InvalidOutputFormat;
            return std::ptr::null_mut();
        }
    }

    let content = unsafe { slice::from_raw_parts(content, content_len) };
    let img = match image::load_from_memory(content) {
        Ok(img) => img,
        Err(_) => unsafe {
            (*ctx).status = ImagoStatus::LoadFailed;
            return std::ptr::null_mut();
        },
    };

    let pipeline = unsafe { slice::from_raw_parts(operations_data, operations_len) };

    let transformed = pipeline.iter().fold(img, apply_operation);

    let output_format = unsafe { output_format.read() };

    let (buf, buflen, _) = match encode_image(&transformed, &output_format) {
        Ok(raw) => raw.into_raw_parts(),
        Err(status) => unsafe {
            *ctx = Context {
                status,
                output_len: 0,
            };
            return std::ptr::null_mut();
        },
    };

    unsafe { (*ctx).output_len = buflen }
    buf
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn destroy_output_buffer(contents: *mut u8, len: usize) {
    unsafe {
        if !contents.is_null() {
            let _ = slice::from_raw_parts(contents, len);
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
