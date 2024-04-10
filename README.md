# Xenon

**Xenon 已被废弃. 有关最新进展，请见 [Xenon ATC-X](https://github.com/Elkeid-me/ATC-X)**.

***

Xenon 是某大学编译原理课程的作业，其目标是把 SysY 编译为 RISC-V 汇编. SysY 是 C 语言的一个子集. 此外，Xenon 包含一些语言扩展：

1. 自定义中缀运算符. 例如：
    ```c
    int f(int x, int y) { return (x - 1) * (y - 1); }
    int main()
    {
        int x = 2, y = 2;
        return x `f` y; // 等价于 return f(x, y);
    }
    ```
2. 各类复合赋值运算符，如 `+=`，`*=`.
3. 复合运算符返回左值，这意味着可以写 `x = y = z;` 这种代码.
4. 自增自减运算符，其中前缀自增自减返回左值. 例如 `++x = 1;`
5. 数组初始化允许使用空初始化列表.
4. 以常量下标取得的常量数组元素可以参与编译期常量表达式求值.

Xenon 的开发仍处于早期阶段，它目前包含：

- 使用有限状态机的预处理器，可以去除源代码中的注释，以及把多个 `\` 结尾的物理行拼接为一个逻辑行.
- 使用 [pest](https://pest.rs) 为解析器生成器，[Koopa IR](https://github.com/pku-minic/koopa) 为中间表示的前端. 这里，[pest](https://pest.rs) 是一种基于解析表达式文法的解析器生成器，[Koopa IR](https://github.com/pku-minic/koopa) 是专为某大学编译器课程开发的中间表示.

目前，Xenon 的工作步骤为：

1. 读入文件，将所有的 `\r\n` 转为 `\n`.
2. 1 中的字符串送入 [`preprocessor`](src/preprocessor.rs)，得到不包含注释的源代码.
3. 2 中的字符串送入 [`SysYParser`](src/frontend/parser.rs)，得到 [pest](https://pest.rs) 中以 `Pairs` 为基础的“动态类型”语法树.
4. [`parser`](src/frontend/parser.rs) 深度优先遍历 3 中的语法树，得到在 [`ast`](src/frontend/ast.rs) 中定义的翻译单元类型.
5. 翻译单元送入 [`checker`](src/frontend/checker.rs)，进行语义分析与检查，例如编译期常量表达式的求值、表达式类型检查、符号重定义检查、数组初始化列表检查等.
6. 检查后的翻译单元送入 [`dump`](src/frontend/dump.rs)，直接生成文本格式的 [Koopa IR](https://github.com/pku-minic/koopa).

Xenon 的 `parser` 与 `checker` 功能完备，`dump` 已经支持至作业要求的 Level 8，但不支持作用域嵌套. 换句话说，`parser` 和 `checker` 可以正常处理下面的代码：
```c
const int x[1] = {};
int main()
{
    int x = x[0];
    int y = x + 1;
    {
        int x = x;
        int z = y + x;
    }
    return x;
}
```
但 `dump` 会出现错误.

[Xenon ATC-X](https://github.com/Elkeid-me/ATC-X) 的目标是：

- [x] 在预处理器中，使用生成器而不是 `String::replace` 将所有的 `\r\n` 转为 `\n`.
- [x] 完全使用生成器的预处理器.
- [x] 合并 `parser` 与 `checker`.
- [x] `parser` 中进行 naive 的表达式化简，见 [EXPS](https://github.com/Elkeid-me/EXPS).
- [x] `parser` 中进行名字重整，以支持嵌套作用域. 例如，上面的代码会被重整化为：
    ```c
    const int _Ax[1] = {};
    int main()
    {
        int _Ix = _Ax[0];
        int _Iy = _Ix + 1;
        {
            int _I_Ix = _Ix;
            int _Iz = _Iy + _I_Ix;
        }
        return _Ix;
    }
    ```
- [ ] 死代码消除.
- [ ] 寄存器分配.
