use crate::error::{ImagoError, ImagoStatus};
use image::{DynamicImage, ImageFormat};
use std::io::Cursor;

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

pub fn decode_image(bytes: &[u8], format: Option<Format>) -> Result<DynamicImage, ImagoError> {
    if let Some(fmt) = format {
        let fmt: ImageFormat = fmt.into();

        image::load_from_memory_with_format(bytes, fmt)
            .or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))
    } else {
        image::load_from_memory(bytes).or_else(|e| Err(ImagoError::new(ImagoStatus::LoadFailed, e)))
    }
}

pub fn encode_image(
    img: &DynamicImage,
    format: &Format,
    quality: Option<u8>,
) -> Result<Vec<u8>, ImagoError> {
    let mut output = Vec::new();
    let mut cursor = Cursor::new(&mut output);

    let image_format: ImageFormat = (*format).into();

    // For formats that support quality settings, use the specific encoder
    if let Some(q) = quality {
        let quality = q.clamp(1, 100);
        match format {
            Format::Jpeg => {
                use image::codecs::jpeg::JpegEncoder;
                let encoder = JpegEncoder::new_with_quality(&mut cursor, quality);
                img.write_with_encoder(encoder)
                    .map_err(|e| ImagoError::new(ImagoStatus::EncodeFailed, e))?;
            }
            Format::WebP => {
                // Use the dedicated webp crate for quality-controlled encoding
                let encoder = webp::Encoder::from_image(img)
                    .map_err(|e| ImagoError::new(ImagoStatus::EncodeFailed, e))?;
                let webp_data = encoder.encode(quality as f32);
                output = webp_data.to_vec();
                // Skip the default cursor writing since we already have the data
                return Ok(output);
            }
            _ => {
                // For formats that don't support quality, fall back to default encoding
                img.write_to(&mut cursor, image_format)
                    .map_err(|e| ImagoError::new(ImagoStatus::EncodeFailed, e))?;
            }
        }
    } else {
        // No quality specified, use default encoding
        img.write_to(&mut cursor, image_format)
            .map_err(|e| ImagoError::new(ImagoStatus::EncodeFailed, e))?;
    }

    Ok(output)
}
