/// Miscellaneous errors
#[derive(Debug)]
pub enum Error {
    /// The choice of the parameters clip_nesting_counter_bits or winding_counter_bits is not supported.
    NumberOfStencilBitsIsUnsupported,
    /// Pushed more than 2 to the power of clip_nesting_counter_bits Shapes onto the ClipStack.
    ClipStackOverflow,
    /// Popped from an empty ClipStack.
    ClipStackUnderflow,
}
