use std::io::Cursor;
use crate::error::{ImagoError, ImagoStatus};
use crate::formats::{ColorType, OptionalFormat};
use image::ImageReader;

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

pub fn get_image_info_inner(path: &str) -> Result<ImageInfo, ImagoError> {
    let file_size = std::fs::metadata(path)
        .or(Err(ImagoError::new(
            ImagoStatus::LoadFailed,
            "Image metadata not found",
        )))?
        .len() as usize;

    let reader =
        ImageReader::open(path).or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?;

    let format = reader.format().map(|fmt| fmt.into());

    let (width, height) = reader
        .into_dimensions()
        .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?;

    let img = ImageReader::open(path)
        .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?
        .decode()
        .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?;

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

pub fn get_buffer_info_inner(bytes: &[u8]) -> Result<ImageInfo, ImagoError> {
    let file_size = bytes.len();

    let cursor = Cursor::new(bytes);
    let mut reader = ImageReader::new(cursor);
    
    // Try to guess the format from the buffer content
    reader = reader.with_guessed_format()
        .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?;

    let format = reader.format().map(|fmt| fmt.into());

    // Get dimensions without fully decoding
    let (width, height) = reader
        .into_dimensions()
        .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?;

    // Now decode the image to get color information
    let cursor2 = Cursor::new(bytes);
    let img = ImageReader::new(cursor2)
        .with_guessed_format()
        .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?
        .decode()
        .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))?;

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
