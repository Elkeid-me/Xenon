use std::iter::zip;
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Void,
    Array(Vec<usize>),
    Pointer(Vec<usize>),
}

impl Type {
    pub fn can_convert_to(&self, rhs: &Self) -> bool {
        match self {
            Type::Int => matches!(rhs, Type::Int),
            Type::Void => matches!(rhs, Type::Int),
            Type::Array(len_1) => match rhs {
                Type::Pointer(len_2) => zip(len_1.iter().skip(1), len_2.iter()).all(|(l, r)| *l == *r),
                Type::Array(len_2) => *len_1 == *len_2,
                _ => false,
            },
            Type::Pointer(len_1) => match rhs {
                Type::Pointer(len_2) => *len_1 == *len_2,
                _ => false,
            },
        }
    }
}
