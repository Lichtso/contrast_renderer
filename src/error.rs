//! Error handling

/// Miscellaneous errors
#[derive(Debug)]
pub enum Error {
    /// The choice of the parameters `clip_nesting_counter_bits` or `winding_counter_bits` is not supported.
    NumberOfStencilBitsIsUnsupported,
    /// Pushed more than 2 to the power of `clip_nesting_counter_bits` [Shape](crate::renderer::Shape)s onto the [ClipStack](crate::renderer::ClipStack).
    ClipStackOverflow,
    /// Popped from an empty [ClipStack](crate::renderer::ClipStack).
    ClipStackUnderflow,
    /// Exceeded the maximum number of [DashInterval](crate::path::DashInterval)s in [DynamicStrokeOptions](crate::path::DynamicStrokeOptions).
    TooManyDashIntervals,
    /// Exceeded the maximum number of [DynamicStrokeOptions](crate::path::DynamicStrokeOptions) in [Shape](crate::renderer::Shape).
    TooManyDynamicStrokeOptionsGroup,
    /// The passed [DynamicStrokeOptions](crate::path::DynamicStrokeOptions) index is invalid.
    DynamicStrokeOptionsIndexOutOfBounds,
}

/// Used for floating point comparison.
pub const ERROR_MARGIN: f32 = 0.0001;
