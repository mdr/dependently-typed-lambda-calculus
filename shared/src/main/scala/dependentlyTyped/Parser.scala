package dependentlyTyped

import dependentlyTyped.Term._

import scala.language.postfixOps
import scala.util.parsing.combinator._

object Parser extends RegexParsers {

  def ident: Parser[String] = "" ~> // handle whitespace
    rep1(acceptIf(c => Character.isJavaIdentifierStart(c) && c != 'λ')("identifier expected but '" + _ + "' found"),
      elem("identifier part", c => Character.isJavaIdentifierPart(c) || c == '\'')) ^^ (_.mkString)

  def parseStatementSafe(s: String): Either[String, Statement] = toEither(parseAll(statement, s))

  def parseTermSafe(s: String): Either[String, InferrableTerm] = toEither(parseAll(term, s))

  def parseTerm(s: String): InferrableTerm = parseTermSafe(s) match {
    case Left(error) => throw new RuntimeException(s"Parser error: $error")
    case Right(term) => term
  }

  private def toEither[T](parseResult: ParseResult[T]): Either[String, T] =
    parseResult match {
      case NoSuccess(message, _) => Left(message)
      case Success(term, _) => Right(term)
    }

  lazy val freeVariable: Parser[FreeVariable] = ident ^^ (name => FreeVariable(name))

  lazy val statement: Parser[Statement] = letStatement | assumeStatement | evalStatement

  lazy val evalStatement: Parser[Statement.Eval] = term ^^ Statement.Eval

  lazy val letStatement: Parser[Statement.Let] = ("let" ~> ident <~ "=") ~ term ^^ {
    case name ~ expression => Statement.Let(name, expression)
  }

  lazy val assumption: Parser[Assumption] = ("(" ~> ident <~ "::") ~ term <~ ")" ^^ {
    case name ~ info => Assumption(name, info)
  }

  lazy val assumeStatement: Parser[Statement.Assume] = ("assume" ~> rep1(assumption)) ^^ Statement.Assume

  lazy val term: Parser[InferrableTerm] = piTerm | maybeAnnotatedTerm

  lazy val maybeAnnotatedTerm: Parser[InferrableTerm] =
    annotatedLambda | maybeFunctionType ~ opt("::" ~> term) ^^ {
      case term ~ Some(typ) => Annotated(term, typ)
      case term ~ None => term
    }

  lazy val annotatedLambda: Parser[InferrableTerm] = parenLambda ~ ("::" ~> term) ^^ {
    case term ~ typ => Annotated(term, typ)
  }

  lazy val parenLambda: Parser[CheckableTerm] = "(" ~> lambdaTerm <~ ")"

  lazy val arrow: Parser[String] = "->" | "→"

  lazy val maybeFunctionType: Parser[InferrableTerm] = maybeApplicationTerm ~ opt(arrow ~> (piTerm | maybeFunctionType)) ^^ {
    case argumentType ~ Some(resultType) => Pi(argumentType, resultType)
    case term ~ None => term
  }

  lazy val natElim: Parser[NatElim] = "natElim_" ~> argument ~ argument ~ argument ~ argument ^^ {
    case motive ~ zeroCase ~ succCase ~ n => NatElim(motive, zeroCase, succCase, n)
  }

  lazy val succ: Parser[Succ] = "Succ_" ~> argument ^^ Succ

  lazy val maybeApplicationTerm: Parser[InferrableTerm] = natElim | succ | simpleTerm ~ rep(argument) ^^ {
    case term ~ List() => term
    case function ~ arguments => arguments.foldLeft(function)((curriedFunction, arg) => Application(curriedFunction, arg))
  }

  lazy val argument: Parser[CheckableTerm] = parenLambda | simpleTerm ^^ Inf

  private def toNumberTerm(n: Int): InferrableTerm = n match {
    case 0 => Term.Zero
    case _ => Term.Succ(toNumberTerm(n - 1))
  }

  lazy val number: Parser[InferrableTerm] = """0|[1-9]\d*""".r ^^ { s => toNumberTerm(s.toInt) }

  lazy val simpleTerm: Parser[InferrableTerm] =
    "Zero" ^^^ Zero | number | ("Nat" | "ℕ") ^^^ Nat | "*" ^^^ * | freeVariable | "(" ~> term <~ ")"

  lazy val lambdaTerm: Parser[CheckableTerm] =
    (("\\" | "λ") ~> rep1(ident) <~ arrow) ~ (lambdaTerm | maybeFunctionType ^^ Inf) ^^ {
      case args ~ body => args.foldRight[CheckableTerm](body)((arg, body) => Lambda(body.substitute(arg, 0)))
    }

  // (x :: *)
  lazy val piArg: Parser[(String, InferrableTerm)] = (("(" ~> ident) <~ "::") ~ term <~ ")" ^^ {
    case arg ~ argumentType => arg -> argumentType
  }

  // forall (x :: *) (y :: Nat) (z :: Vec x y) (a :: Fin y) . x
  lazy val piTerm: Parser[InferrableTerm] = (("∀" | "forall") ~> rep1(piArg) <~ ".") ~ term ^^ {
    case piArgs ~ resultType =>
      piArgs.foldRight[InferrableTerm](resultType) { case ((arg, argumentType), body) => Pi(argumentType, body.substitute(arg, 0)) }
  }

  private implicit class RichInferrableTerm(term: InferrableTerm) {

    def substitute(name: String, i: Int): InferrableTerm = term match {
      case Annotated(term, typ) => Annotated(term.substitute(name, i), typ)
      case BoundVariable(j) => BoundVariable(j)
      case FreeVariable(variableName) =>
        variableName match {
          case Name.Global(variableName) if variableName == name => BoundVariable(i)
          case _ => FreeVariable(variableName)
        }
      case Application(function, argument) => Application(function.substitute(name, i), argument.substitute(name, i))
      case Term.* => *
      case Nat => Nat
      case Zero => Zero
      case Succ(term) => Succ(term.substitute(name, i))
      case NatElim(motive, zeroCase, succCase, n) => NatElim(motive.substitute(name, i), zeroCase.substitute(name, i), succCase.substitute(name, i), n.substitute(name, i))
      case Nil(elementType) => Nil(elementType.substitute(name, i))
      case Cons(elementType, length, head, tail) => Cons(elementType.substitute(name, i), length.substitute(name, i), head.substitute(name, i), tail.substitute(name, i))
      case Vec(elementType, length) => Vec(elementType.substitute(name, i), length.substitute(name, i))
      case Pi(argumentType, resultType) => Pi(argumentType.substitute(name, i), resultType.substitute(name, i + 1))
    }
  }

  private implicit class RichCheckableTerm(term: CheckableTerm) {

    def substitute(name: String, i: Int): CheckableTerm = term match {
      case Inf(term) => Inf(term.substitute(name, i))
      case Lambda(body) => Lambda(body.substitute(name, i + 1))
    }

  }

}
