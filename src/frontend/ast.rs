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
    PointerTmp(String, Vec<Expr>),
    Pointer(String, Vec<usize>),
}

#[derive(Debug)]
pub enum Definition {
    ConstVariableDefinitionTmp(String, Expr),
    ConstVariableDefinition(String, i32),
    ConstArrayDefinitionTmp {
        identifier: String,
        lengths: Vec<Expr>,
        init_list: InitializerList,
    },
    ConstArrayDefinition {
        identifier: String,
        lengths: Vec<usize>,
        init_list: ConstInitializerList,
    },
    VariableDefinition(String, Option<Expr>),
    ArrayDefinitionTmp {
        identifier: String,
        lengths: Vec<Expr>,
        init_list: Option<InitializerList>,
    },
    ArrayDefinition {
        identifier: String,
        lengths: Vec<usize>,
        init_list: Option<InitializerList>,
    },
}

pub type InitializerList = Vec<InitializerListItem>;

#[derive(Debug)]
pub enum InitializerListItem {
    InitializerList(Box<InitializerList>),
    Expr(Expr),
}

pub type ConstInitializerList = Vec<ConstInitializerListItem>;

#[derive(Debug)]
pub enum ConstInitializerListItem {
    InitializerList(Box<ConstInitializerList>),
    Num(i32),
}

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    If {
        condition: Expr,
        then_block: Box<Block>,
        else_block: Box<Block>,
    },
    While {
        condition: Expr,
        block: Box<Block>,
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
    ArrayElement(String, Vec<Expr>),
}
