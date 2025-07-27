#![feature(vec_into_raw_parts)]
use image::imageops::FilterType as ImageFilter;
use image::{DynamicImage, ImageFormat};
use std::ffi::CStr;
use std::io::Cursor;
use std::os::raw::c_char;
use std::slice;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum ImagoStatus {
    OK = 0,
    InvalidPath = 1,
    InvalidInputBuffer = 2,
    InvalidOperation = 3,
    InvalidInputFormat = 4,
    InvalidOutputFormat = 5,
    LoadFailed = 6,
    EncodeFailed = 7,
    NullContext = 8,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ByteArray {
    pub len: usize,
    pub data: *const u8,
}

impl From<Vec<u8>> for ByteArray {
    fn from(value: Vec<u8>) -> Self {
        let (data, len, _) = value.into_raw_parts();
        ByteArray { data, len }
    }
}

trait Arguments {}

struct FileArgs<'a>(&'a str);

impl<'a> Arguments for FileArgs<'a> {}

impl<'a> FileArgs<'a> {
    fn decode(input_path: *const c_char) -> Option<Self> {
        (!input_path.is_null())
            .then(|| unsafe { CStr::from_ptr(input_path) })
            .and_then(|s| s.to_str().ok())
            .map(|input_path| Self(input_path))
    }
}

struct BufferArgs<'a> {
    input_buffer: &'a [u8],
    input_format: Option<Format>,
}

impl<'a> Arguments for BufferArgs<'a> {}

impl<'a> BufferArgs<'a> {
    fn decode(
        buffer_data: *const u8,
        buffer_len: usize,
        input_format: Option<*const Format>,
    ) -> Result<Self, ImagoStatus> {
        let input_buffer = (!buffer_data.is_null())
            .then(|| unsafe { slice::from_raw_parts(buffer_data, buffer_len) })
            .ok_or(ImagoStatus::InvalidInputBuffer)?;

        let input_format = match input_format {
            Some(fmt) => {
                if fmt.is_null() {
                    Err(ImagoStatus::InvalidInputFormat)
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

fn decode_pipeline<'a>(
    operations_data: *const Operation,
    operations_len: usize,
) -> Option<&'a [Operation]> {
    (!operations_data.is_null())
        .then(|| unsafe { slice::from_raw_parts(operations_data, operations_len) })
}

fn decode_output_format(fmt: *const Format) -> Option<Format> {
    (!fmt.is_null()).then(|| unsafe { fmt.read() })
}

#[repr(C)]
struct Request<'a, A: Arguments> {
    args: A,
    pipeline: &'a [Operation],
    output_format: Format,
}

impl<'a> Request<'a, FileArgs<'a>> {
    fn build_file(
        input_path: *const c_char,
        operations_data: *const Operation,
        operations_len: usize,
        output_format: *const Format,
    ) -> Result<Self, ImagoStatus> {
        let args = FileArgs::decode(input_path).ok_or(ImagoStatus::InvalidPath)?;
        let pipeline = decode_pipeline(operations_data, operations_len)
            .ok_or(ImagoStatus::InvalidOperation)?;
        let output_format =
            decode_output_format(output_format).ok_or(ImagoStatus::InvalidOutputFormat)?;

        Ok(Self {
            args,
            pipeline,
            output_format,
        })
    }
}

impl<'a> Request<'a, BufferArgs<'a>> {
    fn build_buffer(
        buffer_data: *const u8,
        buffer_len: usize,
        input_format: Option<*const Format>,
        operations_data: *const Operation,
        operations_len: usize,
        output_format: *const Format,
    ) -> Result<Self, ImagoStatus> {
        let args = BufferArgs::decode(buffer_data, buffer_len, input_format)?;
        let pipeline = decode_pipeline(operations_data, operations_len)
            .ok_or(ImagoStatus::InvalidOperation)?;
        let output_format =
            decode_output_format(output_format).ok_or(ImagoStatus::InvalidOutputFormat)?;

        Ok(Self {
            args,
            pipeline,
            output_format,
        })
    }
}

#[repr(C, u8)]
#[derive(Debug, Clone)]
pub enum Format {
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

fn encode_image(img: &DynamicImage, format: &Format) -> Result<Vec<u8>, ImagoStatus> {
    let mut output = Vec::new();
    let mut cursor = Cursor::new(&mut output);

    match format {
        Format::WebP => img.write_to(&mut cursor, ImageFormat::WebP),
        Format::Jpeg(quality) => {
            let mut encoder = image::codecs::jpeg::JpegEncoder::new_with_quality(cursor, *quality);
            encoder.encode_image(img)
        }
        Format::Png => img.write_to(&mut cursor, ImageFormat::Png),
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

pub struct Context(ImagoStatus);

impl Context {
    fn success() -> Self {
        Self(ImagoStatus::OK)
    }

    fn failure(status: ImagoStatus) -> Self {
        Self(status)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn make_context() -> *mut Context {
    let context = Context(ImagoStatus::OK).into();

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
    unsafe { ctx.read().0 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_image(
    ctx: *mut Context,
    input_path: *const c_char,
    operations_data: *const Operation,
    operations_len: usize,
    output_format: *const Format,
) -> *mut ByteArray {
    let request =
        match Request::build_file(input_path, operations_data, operations_len, output_format) {
            Ok(req) => req,
            Err(status) => unsafe {
                *ctx = Context::failure(status);
                return std::ptr::null_mut();
            },
        };

    let img = match image::open(request.args.0) {
        Ok(img) => img,
        Err(_) => unsafe {
            *ctx = Context::failure(ImagoStatus::LoadFailed);
            return std::ptr::null_mut();
        },
    };

    let transformed = request.pipeline.iter().fold(img, apply_operation);

    let buf = match encode_image(&transformed, &request.output_format) {
        Ok(raw) => raw,
        Err(status) => unsafe {
            *ctx = Context::failure(status);
            return std::ptr::null_mut();
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
    output_format: *const Format,
) -> *mut ByteArray {
    if ctx.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::NullContext);
            return std::ptr::null_mut();
        }
    }

    let input_format = (!input_format.is_null()).then(|| input_format);

    let request = match Request::build_buffer(
        content,
        content_len,
        input_format,
        operations_data,
        operations_len,
        output_format,
    ) {
        Ok(request) => request,
        Err(status) => unsafe {
            *ctx = Context::failure(status);
            return std::ptr::null_mut();
        },
    };

    let img = match image::load_from_memory(request.args.input_buffer) {
        Ok(img) => img,
        Err(_) => unsafe {
            (*ctx) = Context::failure(ImagoStatus::LoadFailed);
            return std::ptr::null_mut();
        },
    };

    let transformed = request.pipeline.iter().fold(img, apply_operation);

    let buf = match encode_image(&transformed, &request.output_format) {
        Ok(raw) => ByteArray::from(raw).into(),
        Err(status) => unsafe {
            *ctx = Context::failure(status);
            return std::ptr::null_mut();
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
