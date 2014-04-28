object Lang {
  trait Exp
  case object F extends Exp
  case object G extends Exp
  case object X extends Exp
  case object Y extends Exp
  case object Z extends Exp

  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mul(lhs: Exp, rhs: Exp) extends Exp
  case class Fun(parameter: Exp, body: Exp) extends Exp
  case class App(operator: Exp, operand: Exp) extends Exp

  val e1: Exp = Add(Mul(X, Y), Mul(X, Z))

  val id: Exp = Fun(X, X)

  val double: Exp = Fun(X, Add(X, X))

  val app: Exp = Fun(F, Fun(X, App(F, X)))

  val omega: Exp = App(Fun(X, App(X, X)), Fun(X, App(X, X)))
}
