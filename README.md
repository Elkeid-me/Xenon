# Xenon

**Legacy Xenon. It's replaced by Xenon-ATC-X.**

Xenon is a toy compiler for course *Compiling Principles* in some university. It compiles SysY to RISC-V assembly.

Xenon consists of a naive preprocessor using FSA, and unimplemented frontend, optimizer and backend.

Xenon 是某大学编译原理课程的玩具级作业. 它把 SysY 编译为 RISC-V 汇编.

Xenon 包含使用有限状态自动机的预处理器, 以及使用 [pest](pest.rs) 为语法分析器生成器的前端、未实现的优化器和后端.

从作业角度考虑，Xenon 的前端已经能处理 Lv8 的内容. 在语义分析时，它可以正常处理下面的代码：

```c
int main()
{
    int x;
    {
        int x;
    }
    return 0;
}
```

但是生成 IR 时会出现错误，因为 Xenon 进行名字重整.
