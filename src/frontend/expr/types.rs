use std::iter::zip;
#[derive(Debug, Clone, Copy)]
pub enum Type<'a> {
    Int,
    Void,
    Array(&'a [usize]),
    Pointer(&'a [usize]),
}

impl Type<'_> {
    pub fn can_convert_to(&self, rhs: &Self) -> bool {
        match self {
            Type::Int => matches!(rhs, Type::Int),
            Type::Void => matches!(rhs, Type::Int),
            Type::Array(len_1) => match rhs {
                Type::Pointer(len_2) if len_1.len() == len_2.len() + 1 => {
                    zip(len_1.iter().skip(1), len_2.iter()).all(|(l, r)| *l == *r)
                }
                Type::Array(len_2) => len_1 == len_2,
                _ => false,
            },
            Type::Pointer(len_1) => match rhs {
                Type::Pointer(len_2) => len_1 == len_2,
                _ => false,
            },
        }
    }
}
