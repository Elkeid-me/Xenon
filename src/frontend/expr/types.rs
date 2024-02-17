#[derive(Debug, Clone, Copy)]
pub enum Type<'a> {
    Int,
    Void,
    Array(&'a [usize]),
    Pointer(&'a [usize]),
}

impl Type<'_> {
    pub fn can_convert_to(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Type::Int, Type::Int) | (Type::Void, Type::Void) => true,
            (Type::Array(len_l), Type::Array(len_r)) => len_l == len_r,
            (Type::Array(len_l), Type::Pointer(len_r)) => &len_l[1..] == *len_r,
            (Type::Pointer(len_l), Type::Pointer(len_r)) => len_l == len_r,
            _ => false,
        }
    }
}
