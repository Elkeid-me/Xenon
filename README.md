# Xenon

**Xenon 已被废弃. 有关最新进展，请见 [ATC-X](https://github.com/Elkeid-me/ATC-X)**.

Xenon 是某大学编译原理课程的玩具级作业. 它把 SysY 编译为 RISC-V 汇编.

Xenon 包含使用有限状态机的预处理器、使用 [pest](pest.rs) 为语法分析器生成器，使用 [Koopa IR](https://github.com/pku-minic/koopa) 为中间表示的前端，以及未实现的优化器和后端.

从作业角度考虑，Xenon 的前端已经能处理 Level 8 的内容，只剩下数组没有处理.

需要注意的是，在语法和语义分析时，Xenon 可以正常处理下面的代码：

```c
int main()
{
    int x = 0;
    int y = x + 1;
    {
        int x = 1;
        int z = y + x;
    }
    return x;
}
```

但是生成 IR 时会出现错误，因为 Xenon 没有实现名字重整.

在 [ATC-X](https://github.com/Elkeid-me/ATC-X) 中，名字重整被这样实现：

```c
int main()
{
    int _Ix = 0;
    int _Iy = _Ix + 1;
    {
        int _I_Ix = 1;
        int _Iz = _Iy + I_Ix;
    }
    return _Ix;
}
```
