#![feature(vec_into_raw_parts)]
use image::imageops::FilterType as ImageFilter;
use image::{DynamicImage, ImageFormat, ImageReader};
use std::ffi::CStr;
use std::io::Cursor;
use std::os::raw::c_char;
use std::slice;

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

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum Format {
    Avif,
    Bmp,
    Dds,
    Farbfeld,
    Gif,
    Hdr,
    Ico,
    Jpeg,
    OpenExr,
    Pcx,
    Png,
    Pnm,
    Qoi,
    Tga,
    Tiff,
    WebP,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum FilterType {
    Nearest,
    Triangle,
    CatmullRom,
    Gaussian,
    Lanczos3,
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
    Horizontal,
    Vertical,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub enum Degree {
    Deg90,
    Deg180,
    Deg270,
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

fn decode_image(bytes: &[u8], format: Option<Format>) -> Result<DynamicImage, ImagoStatus> {
    if let Some(fmt) = format {
        let fmt: ImageFormat = fmt.into();

        image::load_from_memory_with_format(bytes, fmt).or(Err(ImagoStatus::LoadFailed))
    } else {
        image::load_from_memory(bytes).or(Err(ImagoStatus::LoadFailed))
    }
}

fn encode_image(img: &DynamicImage, format: &Format) -> Result<Vec<u8>, ImagoStatus> {
    let mut output = Vec::new();
    let mut cursor = Cursor::new(&mut output);

    let image_format: ImageFormat = (*format).into();
    img.write_to(&mut cursor, image_format)
        .map_err(|_| ImagoStatus::EncodeFailed)?;

    Ok(output)
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum ColorType {
    L8,
    L16,
    La8,
    La16,
    Rgb8,
    Rgb16,
    Rgb32F,
    Rgba8,
    Rgba16,
    Rgba32F,
}

impl From<Format> for ImageFormat {
    fn from(format: Format) -> Self {
        match format {
            Format::Avif => ImageFormat::Avif,
            Format::Bmp => ImageFormat::Bmp,
            Format::Dds => ImageFormat::Dds,
            Format::Farbfeld => ImageFormat::Farbfeld,
            Format::Gif => ImageFormat::Gif,
            Format::Hdr => ImageFormat::Hdr,
            Format::Ico => ImageFormat::Ico,
            Format::Jpeg => ImageFormat::Jpeg,
            Format::OpenExr => ImageFormat::OpenExr,
            Format::Pcx => ImageFormat::Pcx,
            Format::Png => ImageFormat::Png,
            Format::Pnm => ImageFormat::Pnm,
            Format::Qoi => ImageFormat::Qoi,
            Format::Tga => ImageFormat::Tga,
            Format::Tiff => ImageFormat::Tiff,
            Format::WebP => ImageFormat::WebP,
        }
    }
}

impl From<ImageFormat> for Format {
    fn from(format: ImageFormat) -> Self {
        match format {
            ImageFormat::Avif => Format::Avif,
            ImageFormat::Bmp => Format::Bmp,
            ImageFormat::Dds => Format::Dds,
            ImageFormat::Farbfeld => Format::Farbfeld,
            ImageFormat::Gif => Format::Gif,
            ImageFormat::Hdr => Format::Hdr,
            ImageFormat::Ico => Format::Ico,
            ImageFormat::Jpeg => Format::Jpeg,
            ImageFormat::OpenExr => Format::OpenExr,
            ImageFormat::Pcx => Format::Pcx,
            ImageFormat::Png => Format::Png,
            ImageFormat::Pnm => Format::Pnm,
            ImageFormat::Qoi => Format::Qoi,
            ImageFormat::Tga => Format::Tga,
            ImageFormat::Tiff => Format::Tiff,
            ImageFormat::WebP => Format::WebP,
            _ => panic!(
                "Unsupported ImageFormat: {:?}. This should never happen, please open an issue or contact faycarsons23@gmail.com",
                format
            ),
        }
    }
}

impl From<image::ColorType> for ColorType {
    fn from(color: image::ColorType) -> Self {
        match color {
            image::ColorType::L8 => ColorType::L8,
            image::ColorType::L16 => ColorType::L16,
            image::ColorType::La8 => ColorType::La8,
            image::ColorType::La16 => ColorType::La16,
            image::ColorType::Rgb8 => ColorType::Rgb8,
            image::ColorType::Rgb16 => ColorType::Rgb16,
            image::ColorType::Rgb32F => ColorType::Rgb32F,
            image::ColorType::Rgba8 => ColorType::Rgba8,
            image::ColorType::Rgba16 => ColorType::Rgba16,
            image::ColorType::Rgba32F => ColorType::Rgba32F,
            _ => panic!(
                "Unsupported ColorType: {:?}. This should never happen, please open an issue or contact faycarsons23@gmail.com",
                color
            ),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct OptionalFormat {
    pub has_value: bool,
    pub value: Format,
}

impl OptionalFormat {
    pub fn some(format: Format) -> Self {
        Self {
            has_value: true,
            value: format,
        }
    }
    
    pub fn none() -> Self {
        Self {
            has_value: false,
            value: Format::Png, // Default value, ignored when has_value is false
        }
    }
}

impl From<Option<Format>> for OptionalFormat {
    fn from(opt: Option<Format>) -> Self {
        match opt {
            Some(format) => OptionalFormat::some(format),
            None => OptionalFormat::none(),
        }
    }
}

#[repr(C)]
pub struct ImageInfo {
    pub width: u32,
    pub height: u32,
    pub format: OptionalFormat,
    pub color: ColorType,
    pub file_size: usize,
    pub has_alpha: bool,
    pub aspect_ratio: f64,
}

#[unsafe(no_mangle)]
pub extern "C" fn get_image_info(ctx: *mut Context, input_path: *const c_char) -> *mut ImageInfo {
    if ctx.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::NullContext);
            return std::ptr::null_mut();
        }
    }

    if input_path.is_null() {
        unsafe {
            *ctx = Context::failure(ImagoStatus::InvalidPath);
            return std::ptr::null_mut();
        }
    }

    let path = unsafe {
        match CStr::from_ptr(input_path).to_str() {
            Ok(path) => path,
            Err(_) => {
                *ctx = Context::failure(ImagoStatus::InvalidPath);
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
        Err(status) => unsafe {
            *ctx = Context::failure(status);
            std::ptr::null_mut()
        },
    }
}

fn get_image_info_inner(path: &str) -> Result<ImageInfo, ImagoStatus> {
    let file_size = std::fs::metadata(path)
        .or(Err(ImagoStatus::LoadFailed))?
        .len() as usize;

    let reader = ImageReader::open(path).or(Err(ImagoStatus::LoadFailed))?;

    let format = reader.format().map(|fmt| fmt.into());

    let (width, height) = reader.into_dimensions().or(Err(ImagoStatus::LoadFailed))?;

    let img = ImageReader::open(path)
        .or(Err(ImagoStatus::LoadFailed))?
        .decode()
        .or(Err(ImagoStatus::LoadFailed))?;

    let color = img.color();

    let has_alpha = {
        use image::ColorType::*;

        match color {
            La8 | La16 | Rgba8 | Rgba16 | Rgba32F => true,
            _ => false,
        }
    };

    let aspect_ratio = width as f64 / height as f64;

    Ok(ImageInfo {
        width,
        height,
        format: format.into(),
        color: color.into(),
        file_size,
        has_alpha,
        aspect_ratio,
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn destroy_image_info(image_info: *mut ImageInfo) {
    if !image_info.is_null() {
        unsafe {
            let _ = Box::from_raw(image_info);
        }
    }
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

    let img = match decode_image(request.args.input_buffer, request.args.input_format) {
        Ok(img) => img,
        Err(status) => unsafe {
            *ctx = Context::failure(status);
            return std::ptr::null_mut();
        },
    };

    match image::load_from_memory(request.args.input_buffer) {
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
