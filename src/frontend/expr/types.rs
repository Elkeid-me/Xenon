
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Void,
    Array(usize),
    Pointer,
}
