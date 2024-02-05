pub type TranslationUnit = Vec<Box<GlobalItem>>;

#[derive(Debug)]
pub enum GlobalItem {
    Definition(Definition),
    FunctionDefinition {
        return_void: bool,
        identifier: String,
        parameter_list: Vec<Parameter>,
        block: Block,
    },
}

#[derive(Debug)]
pub enum Parameter {
    Int(String),
    Pointer(String),
    // Pointer(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum Definition {
    ConstVariableDefinition(String, Expr),
    ConstArrayDefinition {
        identifier: String,
        // lengths: Vec<Expr>,
        length: Expr,
        init_list: InitializerList,
    },
    VariableDefinition(String, Option<Expr>),
    ArrayDefinition {
        identifier: String,
        // lengths: Vec<Expr>,
        length: Expr,
        init_list: Option<InitializerList>,
    },
}

pub type InitializerList = Vec<Expr>;

// #[derive(Debug)]
// pub enum InitializerListItem {
//     InitializerList(Box<InitializerList>),
//     Expr(Expr),
// }

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    If {
        condition: Expr,
        then_block: Block,
        else_block: Block,
    },
    While {
        condition: Expr,
        block: Block,
    },
    Return(Option<Expr>),
    Break,
    Continue,
}

pub type Block = Vec<BlockItem>;

#[derive(Debug)]
pub enum BlockItem {
    Definition(Box<Definition>),
    Block(Box<Block>),
    Statement(Box<Statement>),
}

#[derive(Debug)]
pub enum AssignOp {
    Assignment,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    BitLeftShiftAssign,
    BitRightShiftAssign,
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Multiply,
    Divide,
    Modulus,
    Add,
    Subtract,

    LogicalAnd,
    LogicalOr,

    BitLeftShift,
    BitRightShift,
    BirXor,
    BitAnd,
    BitOr,

    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

#[derive(Debug)]
pub enum InfixOp {
    Assign(AssignOp),
    Arith(ArithmeticOp),
}

#[derive(Debug)]
pub enum ArithmeticUnaryOp {
    LogicalNot,
    Negative,
    Positive,
    BitNot,
}

#[derive(Debug)]
pub enum OtherUnaryOp {
    PostfixSelfIncrease,
    PostfixSelfDecrease,
    PrefixSelfIncrease,
    PrefixSelfDecrease,
    AddressOf,
    Indirection,
}
#[derive(Debug)]
pub enum UnaryOp {
    ArithUnary(ArithmeticUnaryOp),
    Other(OtherUnaryOp),
}

#[derive(Debug)]
pub enum Expr {
    InfixExpr(Box<Expr>, InfixOp, Box<Expr>),
    UnaryExpr(UnaryOp, Box<Expr>),

    Num(i32),
    Identifier(String),
    FunctionCall(String, Vec<Expr>),
    ArrayElement(String, Box<Expr>),
}
