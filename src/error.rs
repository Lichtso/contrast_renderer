#[derive(Debug)]
pub enum Error {
    NumberOfStencilBitsIsUnsupported,
    ClipStackOverflow,
    ClipStackUnderflow,
}
