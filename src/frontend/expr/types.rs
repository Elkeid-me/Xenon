#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Void,
    Array(usize),
    Pointer,
}

impl Type {
    pub fn can_convert_to(&self, rhs: &Self) -> bool {
        match self {
            Type::Int => matches!(rhs, Type::Int),
            Type::Void => matches!(rhs, Type::Int),
            Type::Array(len_1) => match rhs {
                Type::Pointer => true,
                Type::Array(len_2) if len_1 == len_2 => true,
                _ => false,
            },
            Type::Pointer => matches!(rhs, Type::Pointer),
        }
    }
}
