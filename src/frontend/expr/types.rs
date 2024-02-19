#[derive(Debug, Clone, Copy)]
pub enum Type<'a> {
    Int,
    Void,
    Pointer(&'a [usize]),
}

impl Type<'_> {
    pub fn can_convert_to(&self, rhs: &Self) -> bool {
        match (*self, *rhs) {
            (Type::Int, Type::Int) | (Type::Void, Type::Void) => true,
            (Type::Pointer(l_1), Type::Pointer(l_2)) => l_1 == l_2,
            _ => false,
        }
    }
}
