package dependentlyTyped

import dependentlyTyped.Term._

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.parsing.combinator._

object Parser extends RegexParsers {

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

  lazy val ident: Regex = "[a-zA-Z]+".r

  lazy val freeVariable: Parser[FreeVariable] = ident ^^ (name => FreeVariable(name))

  lazy val statement: Parser[Statement] = letStatement | assumeStatement | term ^^ Statement.Eval

  lazy val letStatement: Parser[Statement.Let] = ("let" ~> ident <~ "=") ~ term ^^ {
    case name ~ expression => Statement.Let(name, expression)
  }

  lazy val assumeStatement: Parser[Statement.Assume] = ("assume" ~> ident <~ "::") ~ term ^^ {
    case name ~ info => Statement.Assume(name, info)
  }

  lazy val term: Parser[InferrableTerm] = maybeAnnotatedTerm

  lazy val maybeAnnotatedTerm: Parser[InferrableTerm] =
    (maybeApplication ~ opt("::" ~> term) ^^ {
      case term ~ Some(typ) => Annotated(term, typ)
      case term ~ None => term
    }) | (lambdaTerm ~ ("::" ~> term) ^^ {
      case term ~ typ => Annotated(term, typ)
    })

  lazy val maybeApplication: Parser[InferrableTerm] = simpleTerm ~ rep(applicationArg) ^^ {
    case term ~ Nil => term
    case function ~ arguments => arguments.foldLeft(function)((curriedFunction, arg) => Application(curriedFunction, arg))
  }

  lazy val applicationArg: Parser[CheckableTerm] = lambdaTerm | (simpleTerm ^^ Inf)

  lazy val simpleTerm: Parser[InferrableTerm] = freeVariable | "(" ~> term <~ ")"

  lazy val lambdaTerm: Parser[CheckableTerm] = ("(" ~> (("\\" | "λ") ~> rep1(ident) <~ "->") ~ maybeApplication) <~ ")" ^^ {
    case args ~ body => args.foldRight[CheckableTerm](Inf(body))((arg, body) => Lambda(body.substitute(arg, 0)))
  }

  // (x :: *)
  lazy val piArg: Parser[(String, InferrableTerm)] = "(" ~> ident ~> "::" ~ term <~ ")" ^^ {
    case arg ~ argumentType => arg -> argumentType
  }

  // forall (x :: *) (y :: Nat) (z :: Vec x y) (a :: Fin y) . x
  lazy val pi: Parser[Pi] = (("∀" | "forall") ~> rep1(piArg) <~ ".") ~ term ^^ {
    case piArgs ~ resultType =>
      piArgs.foldRight[InferrableTerm](resultType) { case ((arg, argumentType), body) => Pi(argumentType, body.substitute(arg, 0)) }.asInstanceOf[Pi]
  }

  private implicit class RichInferrableTerm(term: InferrableTerm) {

    def substitute(name: String, i: Int): InferrableTerm = term match {
      case Annotated(term, typ) => Annotated(term.substitute(name, i), typ)
      case BoundVariable(j) => BoundVariable(j)
      case FreeVariable(Name.Global(variableName)) if variableName == name => BoundVariable(i)
      case FreeVariable(name) => FreeVariable(name)
      case Application(function, argument) => Application(function.substitute(name, i), argument.substitute(name, i))
    }
  }

  private implicit class RichCheckableTerm(term: CheckableTerm) {

    def substitute(name: String, i: Int): CheckableTerm = term match {
      case Inf(term) => Inf(term.substitute(name, i))
      case Lambda(body) => Lambda(body.substitute(name, i + 1))
    }

  }

  //  def typ: Parser[InferrableTerm] =
  //    rep1sep(simpleType, "->") ^^
  //      (_.reduceRight[InferrableTerm] { case (functionType, argumentType) => FunctionType(functionType, argumentType) })

  //  def simpleType: Parser[Type] = freeType | "(" ~> typ <~ ")"

  //  def freeType: Parser[FreeType] = ident ^^ (name => FreeType(name))

}
