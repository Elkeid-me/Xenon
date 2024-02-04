
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Void,
    Array(Box<Type>, Vec<usize>),
    Pointer(Box<Type>),
}
