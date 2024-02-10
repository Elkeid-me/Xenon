pub type TranslationUnit = Vec<Box<GlobalItem>>;

#[derive(Debug)]
pub enum GlobalItem {
    Def(Definition),
    FuncDef {
        return_void: bool,
        id: String,
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
    ConstVariableDefTmp(String, Expr),
    ConstVariableDef(String, i32),
    ConstArrayDefTmp {
        id: String,
        lengths: Vec<Expr>,
        init_list: InitList,
    },
    ConstArrayDef {
        id: String,
        lengths: Vec<usize>,
        init_list: ConstInitList,
    },
    VariableDef(String, Option<Expr>),
    ArrayDefTmp {
        id: String,
        lengths: Vec<Expr>,
        init_list: Option<InitList>,
    },
    ArrayDef {
        id: String,
        lengths: Vec<usize>,
        init_list: Option<InitList>,
    },
}

pub type InitList = Vec<InitListItem>;

#[derive(Debug)]
pub enum InitListItem {
    InitList(Box<InitList>),
    Expr(Expr),
}

pub type ConstInitList = Vec<ConstInitListItem>;

#[derive(Debug)]
pub enum ConstInitListItem {
    InitList(Box<ConstInitList>),
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
    Def(Box<Definition>),
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

impl Default for Expr {
    fn default() -> Self {
        Expr::Num(0)
    }
}
