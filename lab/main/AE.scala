trait AE {
  sealed trait AE {
    final def unparse: String = {
      object Unparse extends TreeConversions
      import Unparse._
      (this: Concrete).unparse
    }
  }

  case class Num(n: Int) extends AE
  case class Var(x: Symbol) extends AE
  case class Add(lhs: AE, rhs: AE) extends AE
  case class Mul(lhs: AE, rhs: AE) extends AE

  object AE {
    object parse extends (String => Either[Failure[AE], AE]) with TreeConversions {
      def apply(code: String): Either[Failure[AE], AE] = Concrete parse code match {
        case Left(Failure(message, context)) => Left(Failure(message, context))
        case Right(concrete) => Right(concrete) // implicit conversion
      }
    }

    def apply(code: String): AE = parse(code) match {
      case Right(x) => x
      case Left(Failure(message, context)) => sys error s"$message\n\ncontext:\n$context"
    }
  }

  // LA: Left-Associative
  trait Unparse { def unparse: String }
  sealed trait Concrete extends Unparse
  sealed trait Summand extends Concrete
  sealed trait Multiplicand extends Concrete
  sealed trait MultiplicandStar extends Unparse
  sealed trait MultiplicandPlus extends MultiplicandStar with Unparse
  sealed trait SummandStar extends Unparse
  sealed trait SummandPlus extends SummandStar with Unparse

  object Concrete extends DelegateM[Concrete] {
    def delegate = NumC <|> VarC <|> ParenC <|> MulC <|> AddC

    def parse(code: String): Either[Failure[Concrete], Concrete] = {
      val tokens = tokenize(code)
      run(tokens) match {
        case failure: Failure[Concrete] =>
          Left(failure)

        case Success(iterator) =>
          val results = collectMostParsed[Concrete](Int.MaxValue, identity, iterator)
          assert(! results.isEmpty) // collectMostParsed is guaranteed to return
          def printResults = results.map(_.get).mkString("\n")
          (results.head.remaining.isEmpty, results.tail.isEmpty) match {
            case (true, true) =>
              Right(results.head.get)

            case (true, false) =>
              Left(Failure(s"ambiguous parses:\n$printResults", tokens))

            case (false, _) =>
              Left(Failure(s"parse incomplete. best matches:\n$printResults", tokens))
          }
      }
    }

    // collect results that are parsed as much as possible
    def collectMostParsed[A](current: Int, acc: List[ParserState[A]] => List[ParserState[A]],
                             raw: Iterator[ParserState[A]]): List[ParserState[A]] =
      if (raw.hasNext) {
        val next @ ParserState(result, remaining) = raw.next
        val remains = remaining.length
        if (remains == current)
          collectMostParsed[A](current, acc compose (next :: _), raw)
        else if (remains < current)
          collectMostParsed[A](remains, next :: _, raw)
        else // remains > current
          collectMostParsed[A](current, acc, raw)
      }
      else // ! raw.hasNext
        acc(Nil)
  }

  case class NumC(n: String) extends Concrete with Summand with Multiplicand {
    def unparse = n.toString
  }

  object NumC extends RegexM[NumC]("number", "[0-9]+")

  case class VarC(x: String) extends Concrete with Summand with Multiplicand {
    def unparse = x
  }

  object VarC extends RegexM[VarC]("identifier", "[a-zA-Z][a-zA-Z0-9_]*")

  case class ParenC(get: Concrete) extends Concrete with Summand with Multiplicand {
    def unparse = s"(${get.unparse})"
  }

  object ParenC extends DelegateM[ParenC] {
    sealed trait LP
    sealed trait RP
    object LP extends LiteralM[LP]("(") with LP
    object RP extends LiteralM[RP](")") with RP
    def delegate = LP ~> Concrete <~ RP map ParenC.apply
  }

  case class MulC(first: Multiplicand, rest: MultiplicandPlus) extends Concrete with Summand {
    def unparse = first.unparse + rest.unparse
  }

  object MulC extends BinOpM[Multiplicand, MultiplicandPlus, MulC](Multiplicand, MultiplicandPlus)

  object Multiplicand extends DelegateM[Multiplicand] {
    def delegate = NumC <|> VarC <|> ParenC
  }

  sealed trait Asterisk extends Unparse

  case object Asterisk extends LiteralM[Asterisk]("*") with Asterisk {
    def unparse = "*"
  }

  case object ZeroMultiplicand extends MultiplicandStar with EmptyM[MultiplicandStar] {
    def unparse = ""
  }

  object MultiplicandStar extends DelegateM[MultiplicandStar] {
    def delegate = ZeroMultiplicand <|> MoreMultiplicand
  }

  case class MoreMultiplicand(asterisk: Asterisk, multiplicand: Multiplicand, others: MultiplicandStar)
      extends MultiplicandStar with MultiplicandPlus {
    def unparse = s" ${asterisk.unparse} ${multiplicand.unparse}${others.unparse}"
  }

  object MoreMultiplicand extends ParserM[MoreMultiplicand] {
    def run = (for {
      asterisk <- Asterisk
      multiplicand <- Multiplicand
      others <- MultiplicandStar
    } yield MoreMultiplicand(asterisk, multiplicand, others)) run _
  }

  object MultiplicandPlus extends DelegateM[MultiplicandPlus] {
    def delegate = MoreMultiplicand
  }

  case class AddC(first: Summand, rest: SummandPlus) extends Concrete {
    def unparse = first.unparse + rest.unparse
  }

  object AddC extends BinOpM[Summand, SummandPlus, AddC](Summand, SummandPlus)

  object Summand extends DelegateM[Summand] {
    def delegate = NumC <|> VarC <|> ParenC <|> MulC
  }

  sealed trait Plus extends Unparse

  case object Plus extends LiteralM[Plus]("+") with Plus {
    def unparse = "+"
  }

  object SummandStar extends DelegateM[SummandStar] {
    def delegate = ZeroSummand <|> MoreSummand
  }

  case object ZeroSummand extends SummandStar with EmptyM[SummandStar] {
    def unparse = ""
  }

  case class MoreSummand(plus: Plus, summand: Summand, others: SummandStar)
      extends SummandStar with SummandPlus {
    def unparse = s" ${plus.unparse} ${summand.unparse}${others.unparse}"
  }

  object MoreSummand extends ParserM[MoreSummand] {
    def run = (for {
      plus <- Plus
      summand <- Summand
      others <- SummandStar
    } yield MoreSummand(plus, summand, others)) run _
  }

  object SummandPlus extends DelegateM[SummandPlus] {
    def delegate = MoreSummand
  }

  // parsers
  abstract class BinOpM[A, B, C](lhs: ParserM[A], rhs: ParserM[B]) extends DelegateM[C] {
    def apply(lhs: A, rhs: B): C
    def delegate = (for { a <- lhs ; b <- rhs } yield apply(a, b))
  }

  trait DelegateM[A] extends ParserM[A] {
    def delegate: ParserM[A]

    def run = cachedDelegate run _

    lazy val cachedDelegate = delegate
  }

  trait EmptyM[A] extends ParserM[A] {
    self: A =>
    def run = toks => ParseResult(this, toks)
  }

  abstract class RegexM[A](nonterminal: String, pattern: String) extends ParserM[A] {
    def apply(string: String): A

    def run = _ match {
      case head :: tail if head.get matches pattern =>
        ParseResult(apply(head.get), tail)

      case otherwise =>
        Failure(s"expect $nonterminal", otherwise)
    }
  }

  class LiteralM[A](literal: String) extends RegexM[A](literal, java.util.regex.Pattern.quote(literal)) {
    self: A =>
    def apply(string: String): A = this
  }

  // BEGIN TOKENIZER
  def keyChars = List("""\[""", """\]""", """\(""", """\)""", """\{""", """\}""")

  // a keychar is always a token.
  // split before a keychar if it's not the first char,
  // split after a keychar if it's not the last char.
  final val keyCharLookaround: String = keyChars.map(char => s"((?!$$)(?<=$char))|(?<!^)(?=$char)").mkString("|")

  case class Token(get: String)

  def tokenize(s: String): List[Token] =
    (for {
      segment <- s split """\s+"""
      token <- segment split keyCharLookaround
    } yield Token(token)).toList
  // END TOKENIZER


  sealed trait ParseResult[+A]

  object ParseResult {
    def apply[A](a: A, rest: List[Token]): ParseResult[A] =
      Success(Iterator(ParserState(a, rest)))
  }

  case class ParserState[+A](get: A, remaining: List[Token])
  case class Success[+A](results: Iterator[ParserState[A]]) extends ParseResult[A]
  case class Failure[+A](message: String, context: List[Token]) extends ParseResult[A]

  case class DefaultParserM[+A](run: List[Token] => ParseResult[A]) extends ParserM[A]
  trait ParserM[+A] {
    def run: List[Token] => ParseResult[A]

    // bind
    def flatMap[B](f: A => ParserM[B]): ParserM[B] = DefaultParserM[B](
      (tokens: List[Token]) => run(tokens) match {
        case Failure(message, context) =>
          Failure(message, context)

        case Success(results) =>
          var firstFailure: Option[Failure[B]] = None
          val iterator: Iterator[ParserState[B]] = for {
            result <- results
            ParserState(a, remaining) = result
            concatenatedIterator <- f(a) run remaining match {
              case Failure(message, context) =>
                if (firstFailure == None)
                  firstFailure = Some(Failure(message, context))
                Iterator.empty

              case Success(newResults) =>
                newResults
            }
          } yield concatenatedIterator
          if (iterator.isEmpty)
            firstFailure.get // if firstFailure is uninitialized, it should fail here
          else
            Success(iterator)
      })

    def map[B](f: A => B): ParserM[B] = flatMap(a => ParserM(f(a)))

    def ~> [B](andThen: ParserM[B]): ParserM[B] = DefaultParserM((for {
      me <- this
      that <- andThen
    } yield that) run _)

    def <~ [B](andThen: ParserM[B]): ParserM[A] = DefaultParserM((for {
      me <- this
      that <- andThen
    } yield me) run _)

    def <|> [A](orElse: ParserM[A]): ParserM[A] = DefaultParserM(tokens =>
      (this run tokens, orElse run tokens) match {
        case (myFail: Failure[A], _: Failure[A]) =>
          myFail

        case (_: Failure[A], success: Success[A]) =>
          success

        case (success: Success[A], _: Failure[A]) =>
          success

        case (these: Success[A], those: Success[A]) => // to help type checker
          Success(these.results ++ those.results)
      })
  }

  object ParserM {
    // unit
    def apply[A](content: A): ParserM[A] =
      DefaultParserM(tokens => ParseResult(content, tokens))
  }

  trait TreeConversions {
    import scala.language.implicitConversions
    import scala.collection.immutable.StringOps

    def ops(s: String): StringOps = new StringOps(s)

    implicit def toSymbol(s: String): Symbol = Symbol(s)
    implicit def toInt(s: String): Int = ops(s).toInt

    implicit def concrete2e(concrete: Concrete): AE = concrete match {
      case NumC(n) => Num(n)
      case VarC(x) => Var(x)
      case ParenC(e) => e // implicit recursion lol

      case MulC(fst, rest) =>
        def loop(lhs: AE, chain: MultiplicandStar): AE = chain match {
          case ZeroMultiplicand => lhs
          case MoreMultiplicand(Asterisk, multiplicand, others) => loop(Mul(lhs, multiplicand), others)
        }
        loop(fst, rest)

      case AddC(fst, rest) =>
        def loop(lhs: AE, chain: SummandStar): AE = chain match {
          case ZeroSummand => lhs
          case MoreSummand(Plus, summand, others) => loop(Add(lhs, summand), others)
        }
        loop(fst, rest)
    }

    implicit def fromSymbol(s: Symbol): String = s.name
    implicit def fromInt(n: Int): String = n.toString

    implicit def ae2concrete(ae: AE): Concrete = ae match {
      case Num(n) => NumC(n)
      case Var(x) => VarC(x)
      case add: Add => flattenAdd(add)
      case mul: Mul => flattenMul(mul)
    }

    def flattenAdd(add: Add): AddC = {
      def toSummand(ae: AE): Summand = (ae: Concrete) match {
        case s: Summand => s
        case otherwise => ParenC(otherwise)
      }
      def loop(root: AE, acc: SummandPlus): AddC = root match {
        case Add(lhs, rhs) => loop(lhs, MoreSummand(Plus, toSummand(rhs), acc))
        case otherwise => AddC(toSummand(otherwise), acc)
      }
      loop(add.lhs, MoreSummand(Plus, toSummand(add.rhs), ZeroSummand))
    }

    def flattenMul(add: Mul): MulC = {
      def toMultiplicand(ae: AE): Multiplicand = (ae: Concrete) match {
        case s: Multiplicand => s
        case otherwise => ParenC(otherwise)
      }
      def loop(root: AE, acc: MultiplicandPlus): MulC = root match {
        case Mul(lhs, rhs) => loop(lhs, MoreMultiplicand(Asterisk, toMultiplicand(rhs), acc))
        case otherwise => MulC(toMultiplicand(otherwise), acc)
      }
      loop(add.lhs, MoreMultiplicand(Asterisk, toMultiplicand(add.rhs), ZeroMultiplicand))
    }
  }
}
