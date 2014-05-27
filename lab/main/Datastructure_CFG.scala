sealed abstract class Exp

case class NonTerminal(v:Symbol) extends Exp
case class Terminal(v:String) extends Exp

case class Option(v:Exp) extends Exp
case class Star(v:Exp) extends Exp
case class Plus(v:Exp) extends Exp

case class Alternatives(v:Exp*) extends Exp {
  override def toString = "Alternatives(" + v.mkString(", ") + ")"
}
case class Sequence(v:Exp*) extends Exp {
  override def toString = "Sequence(" + v.mkString(", ") + ")"
}

class Grammar(val start:NonTerminal, val rules:Map[NonTerminal, Alternatives]) {
  override def toString = rules.mkString("\n")
}

implicit def symbol2NonTerminal(s:Symbol) = new NonTerminal(s)
implicit def string2Terminal(s:String) = new Terminal(s)


// =================


/* Example: The grammar for plt/ae
 *
 * Exp ::= Parenthesized | Num | Id | Mul | Add
 * Parenthesized ::= ( Exp )
 *
 * Num ::= [0-9]+
 * Id ::= [A-Za-z][A-Za-z0-9_]*
 *
 * Mul ::= Multiplicand * Multiplicand
 * Multiplicand ::= Parenthesized | Num | Id
 *
 * Add ::= Summand + Summand
 * Summand ::= Parenthesized | Num | Id | Mul
 */

val ae = new Grammar('Exp, Map(
  NonTerminal('Exp) -> Alternatives('Parenthesized, 'Num, 'Id, 'Mul, 'Add),
  NonTerminal('Parenthesized) -> Alternatives(Sequence("(",'Exp,")")),

  NonTerminal('Num) -> Alternatives(Plus('Digit)),
  NonTerminal('Id) -> Alternatives(Sequence('Char,Star(Alternatives('Char,'Digit)))),
  // Ranges are not yet implemented
  NonTerminal('Digit) -> Alternatives("0","1","2","3","4","5","6","7","8","9"),
  NonTerminal('Char) -> Alternatives("a","b","c"), // you get the gist

  NonTerminal('Mul) -> Alternatives(Sequence('Multiplicand, "*", 'Multiplicand)),
  NonTerminal('Multiplicand) -> Alternatives('Parenthesized, 'Num, 'Id),

  NonTerminal('Add) -> Alternatives(Sequence('Summand, "+", 'Summand)),
  NonTerminal('Summand) -> Alternatives('Parenthesized, 'Num, 'Id, 'Mul)
))

