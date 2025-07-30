use image::DynamicImage;
use image::imageops::FilterType as ImageFilter;

use crate::Format;

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
    Quality(u8), // Image quality 1-100
    Convert(Format),
}

/// State held while interpreting operations slice
pub struct Interpreter {
    pub img: DynamicImage,
    pub quality: Option<u8>, // Image quality 1-100
    pub output_format: Option<Format>,
}

impl Interpreter {
    pub fn new(img: DynamicImage) -> Self {
        Self {
            img,
            quality: None,
            output_format: None,
        }
    }
}

/// Interpret an `Operation` as an actual image transformation and apply it to `img`
pub fn apply_operation(mut state: Interpreter, operation: &Operation) -> Interpreter {
    match operation {
        Operation::Resize {
            w,
            h,
            filter,
            exact,
        } => Interpreter {
            img: if *exact {
                state.img.resize_exact(*w, *h, (*filter).into())
            } else {
                state.img.resize(*w, *h, (*filter).into())
            },
            ..state
        },
        Operation::Rotate(Degree::Deg90) => Interpreter {
            img: state.img.rotate90(),
            ..state
        },
        Operation::Rotate(Degree::Deg180) => Interpreter {
            img: state.img.rotate180(),
            ..state
        },
        Operation::Rotate(Degree::Deg270) => Interpreter {
            img: state.img.rotate270(),
            ..state
        },
        Operation::Flip(Direction::Horizontal) => Interpreter {
            img: state.img.fliph(),
            ..state
        },
        Operation::Flip(Direction::Vertical) => Interpreter {
            img: state.img.flipv(),
            ..state
        },
        Operation::GrayScale => Interpreter {
            img: DynamicImage::ImageLuma8(state.img.to_luma8()),
            ..state
        },
        Operation::Blur(radius) => Interpreter {
            img: state.img.blur(*radius),
            ..state
        },
        Operation::Brighten(delta) => Interpreter {
            img: state.img.brighten(*delta),
            ..state
        },
        Operation::Contrast(factor) => Interpreter {
            img: state.img.adjust_contrast(*factor),
            ..state
        },
        Operation::Quality(quality) => {
            state.quality = Some(*quality);
            state
        }
        Operation::Convert(fmt) => {
            state.output_format = Some(*fmt);
            state
        }
    }
}
