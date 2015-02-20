scala里的基本类型和Java非常类似。虽然scala里这些基本类型也是对象，但用起来和Java的内建类型一模一样。

scala提供了一些比较好用的功能。譬如说多行字符串。

```scala
// scala里使用"""来定义多行字符串
// 这一行的结果往往不是我们想要的。
// 我们人物Type会和Welcome对齐
// 而实际上Type前面的那些空格也会输出
println("""Welcome to Ultamix 3000.
               Type "HELP" for help.""")

// scala里提供了另一种方法来对齐
// TODO 这段代码没明白, """里的是raw string
// 而stripMargin是定义在StringLike里的函数
// 猜测raw string也是一个StringLike
println("""|Welcome to Ultamix 3000.
               |Type "HELP" for help.""".stripMargin)
```

scala为基本类型提供了相当丰富的操作符。譬如说1＋2，实际上就是调用对象1的＋操作符。
scala中的操作符分为三种，中缀（infix）、前缀（prefix）和后缀（postfix）。

前缀用的不多，就是前面一个操作符，后面一个表达式。像-2，!flag，+n，~n。＋不知道有什么用。

中缀见的最多，就是前一个表达式，中间一个操作符，后面一个表达式，1＋2就是。
中缀又分左关联和右关联两种，a op b。如果操作符以:结尾那么就是右关联，a op b解释为b.op(a)。
其它的任何不以:结尾的操作符则为左关联，a op b解释为a.op(b)。

```
"string".indexOf('t')
// 也可以写为
“string" indexOf 't'

"string".indexOf('t', 2)
// 也可以写为
“string" indexOf ('t', 2)
```

后缀就是前面一个表达式，后面一个操作符/函数。任何没有参数的函数都可以用作后缀。

```
"string".toUpperCase()
// 也可以写为
“string" toUpperCase
```

在Java里==在原始类型比较值是否相等。对于对象比较是否是同一个对象，而实际上这种情况实在是用的比较少。
大部分时候我们需要比较的是里面的内容是否相等，那在Java里就要使用equals。scala里==表示的就是对象内容是否相等。