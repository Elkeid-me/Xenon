pub type identifier = String;

pub type TranslationUnit = Vec<GlobalItem>;

pub enum GlobalItem {
    ConstDecl(ConstDecl),
    FuncDecl,
    FuncDef,
    Decl,
}

pub enum ConstDecl {}

pub enum AstNode {
    ast_node,
}
