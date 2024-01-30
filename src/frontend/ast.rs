pub type TranslationUnit = Vec<Box<GlobalItem>>;

#[derive(Debug)]
pub enum GlobalItem {
    Declaration(Declarations),
    FunctionDefinition {
        return_void: bool,
        identifier: String,
        parameter_list: Vec<Parameter>,
        block: Block,
    },
    EOI,
}

#[derive(Debug)]
pub enum Parameter {
    Int(String),
    Array(String, Vec<Expr>),
}

pub type Declarations = Vec<DeclarationItem>;

#[derive(Debug)]
pub enum DeclarationItem {
    ConstVariableDefinition(String, Expr),
    ConstArrayDefinition {
        identifier: String,
        lengths: Vec<Expr>,
        init_list: InitializerList,
    },
    VariableDefinition(String, Option<Expr>),
    ArrayDefinition {
        identifier: String,
        lengths: Vec<Expr>,
        init_list: Option<InitializerList>,
    },
    FunctionDeclaration,
}

pub type InitializerList = Vec<InitializerListItem>;

#[derive(Debug)]
pub enum InitializerListItem {
    InitializerList(Box<InitializerList>),
    Expr(Expr),
}

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
    Declarations(Box<Declarations>),
    Block(Box<Block>),
    Statement(Box<Statement>),
}

#[derive(Debug)]
pub enum Expr {
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
    Modulus(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Subtract(Box<Expr>, Box<Expr>),

    LogicalAnd(Box<Expr>, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),

    BitLeftShift(Box<Expr>, Box<Expr>),
    BitRightShift(Box<Expr>, Box<Expr>),
    BirXor(Box<Expr>, Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),

    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterOrEqual(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    LessOrEqual(Box<Expr>, Box<Expr>),

    Assignment(Box<Expr>, Box<Expr>),
    AddAssignment(Box<Expr>, Box<Expr>),
    SubtractAssignment(Box<Expr>, Box<Expr>),
    MultiplyAssignment(Box<Expr>, Box<Expr>),
    BitAndAssignment(Box<Expr>, Box<Expr>),
    BitOrAssignment(Box<Expr>, Box<Expr>),
    BitXorAssignment(Box<Expr>, Box<Expr>),
    BitLeftShiftAssignment(Box<Expr>, Box<Expr>),
    BitRightShiftAssignment(Box<Expr>, Box<Expr>),

    PostfixSelfIncrease(Box<Expr>),
    PostfixSelfDecrease(Box<Expr>),

    PrefixSelfIncrease(Box<Expr>),
    PrefixSelfDecrease(Box<Expr>),
    LogicalNot(Box<Expr>),
    Negative(Box<Expr>),
    Positive(Box<Expr>),
    AddressOf(Box<Expr>),
    Indirection(Box<Expr>),
    BitNot(Box<Expr>),

    Num(i32),
    Identifier(String),
    FunctionCall(String, Vec<Expr>),
    ArrayElement(String, Vec<Expr>),
}
