//! Error handling

/// Miscellaneous errors
#[derive(Debug)]
pub enum Error {
    /// The choice of the parameters `clip_nesting_counter_bits` or `winding_counter_bits` is not supported.
    NumberOfStencilBitsIsUnsupported,
    /// Trying to render with more than 2 to the power of `clip_nesting_counter_bits` nested clip [Shape](crate::renderer::Shape)s.
    ClipStackOverflow,
    /// Trying to render with more than `opacity_layer_count` nested opacity groups
    TooManyNestedOpacityGroups,
    /// Exceeded the maximum number of [DashInterval](crate::path::DashInterval)s in [DynamicStrokeOptions](crate::path::DynamicStrokeOptions).
    TooManyDashIntervals,
    /// The passed [DynamicStrokeOptions](crate::path::DynamicStrokeOptions) index is invalid.
    DynamicStrokeOptionsIndexOutOfBounds,
}

/// Used for floating point comparison.
pub const ERROR_MARGIN: f32 = 0.0001;
