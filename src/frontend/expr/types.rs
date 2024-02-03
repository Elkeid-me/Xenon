
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Void,
    Unknown,
    Array(Box<Type>, Vec<usize>),
    Pointer(Box<Type>),
    Function(Box<Type>, Vec<Box<Type>>),
}
