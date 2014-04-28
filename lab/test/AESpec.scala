import org.scalatest._
import scala.language.implicitConversions

class AESpec extends FlatSpec with AE {
  def shouldFail(code: String): Throwable = intercept[Throwable](AE(code))

  implicit class ParseString(code: String) {
    def -> (tree: AE): Unit = assert(AE(code) == tree)
  }

  implicit def intToNum(n: Int): Num = Num(n)
  implicit def symbolToVar(s: Symbol): Var = Var(s)

  "AE parser" should "parse well-formed expressions" in {
    "51234" -> Num(51234)
    "x12_xx3" -> Var('x12_xx3)
    "3 * 5" -> Mul(3, 5)
    "x + y" -> Add('x, 'y)
    "(1 * 2) + (3 * (((4))))" -> Add(Mul(1, 2), Mul(3, 4))
    "1 * (2 + 3) * 4" -> Mul(Mul(1, Add(2, 3)), 4)
    "1 * 2 + 3 * 4" -> Add(Mul(1, 2), Mul(3, 4))
  }

  it should "fail on ill-formed expressions" in {
    shouldFail("512x")
    shouldFail("+ 3 5")
    shouldFail("((3)))")
    shouldFail("((x)")
  }

  // AE isn't ambiguous, can't test for ambiguity yet.
}
