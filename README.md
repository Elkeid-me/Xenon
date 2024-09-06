# Xenon

**Xenon 已被废弃. 有关最新进展，请见 [Xenon ATC-X](https://github.com/Elkeid-me/Xenon-ATC-X)**.

***

Xenon 是某大学编译原理课程的作业，其目标是把 SysY 编译为 RISC-V 汇编. SysY 是 C 语言的一个子集. 此外，Xenon 包含一些语言扩展：

Xenon 的开发仍处于早期阶段，它目前包含：

- 使用有限状态机的预处理器，可以去除源代码中的注释，以及把多个 `\` 结尾的物理行拼接为一个逻辑行.
- 使用 [pest](https://pest.rs) 为解析器生成器，[Koopa IR](https://github.com/pku-minic/koopa) 为中间表示的前端.

目前，Xenon 的工作步骤为：

1. 读入文件，将所有的 `\r\n` 转为 `\n`.
2. 1 中的字符串送入 [`preprocessor`](src/preprocessor.rs)，得到不包含注释的源代码.
3. 2 中的字符串送入 [`SysYParser`](src/frontend/parser.rs)，得到 [pest](https://pest.rs) 中以 `Pairs` 为基础的“动态类型”语法树.
4. [`parser`](src/frontend/parser.rs) 深度优先遍历 3 中的语法树，得到在 [`ast`](src/frontend/ast.rs) 中定义的翻译单元类型.
5. 翻译单元送入 [`checker`](src/frontend/checker.rs)，进行语义分析与检查，例如编译期常量表达式的求值、表达式类型检查、符号重定义检查、数组初始化列表检查等.
6. 检查后的翻译单元送入 [`dump`](src/frontend/dump.rs)，直接生成文本格式的 [Koopa IR](https://github.com/pku-minic/koopa).
