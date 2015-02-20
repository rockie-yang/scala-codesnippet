scala为模式设计了case类，以减少额外的无关代码。通常情况下只需要在想要用于模式匹配类前加一个case关键字就可以了。加上case关键字之后，编译器会自动加上一些东西。正因为加上了这些小的支持，使模式匹配相当容易。
```scala
abstract class Expr

// 不需要在参数前面加val关键字
// 有case关键字时val会自动加上
case class Var(name: String) extends Expr

case class Number(num: Double) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String,
                 left: Expr, right: Expr) extends Expr

object MyTest {
    def main(args: Array[String]): Unit = {
        // 工厂方法会自动生成。名字和类名一样
        // 不用每次加上new关键字了
        val v = Var("me")

        // toString方法会自动生成
        // 会打印Var(me)
        println(v)

        // 不要加上new关键字在嵌套定义时特别有用
        // 要不然代码被new关键字占据了大部分视线
        val op = BinOp("+", Number(1), v)
        println(op)

        // 加上case会添加一个copy函数
        // 当我们想从已有对象拷贝而只修改一小部分时有用
        val op2 = op.copy(operator = "-")
        println(op2)
    }
}
```
	<li>