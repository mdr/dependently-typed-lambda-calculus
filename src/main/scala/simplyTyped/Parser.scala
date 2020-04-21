package simplyTyped

import scala.util.parsing.combinator._
import Term._

import scala.language.postfixOps
import scala.util.matching.Regex

object Parser extends RegexParsers {

  def parse(s: String): InferrableTerm = parseSafe(s) match {
    case Left(error) => throw new RuntimeException(s"Parser error: $error")
    case Right(term) => term
  }

  def parseSafe(s: String): Either[String, InferrableTerm] = parseAll(term, s) match {
    case NoSuccess(message, _) => Left(message)
    case Success(term, _) => Right(term)
  }

  def parseType(s: String): Type = parseAll(typ, s).getOrElse(throw new RuntimeException("Parse error"))

  lazy val ident: Regex = "[a-zA-Z]+".r

  lazy val freeVariable: Parser[FreeVariable] = ident ^^ (name => FreeVariable(name))

  lazy val term: Parser[InferrableTerm] = maybeAnnotatedTerm

  lazy val maybeAnnotatedTerm: Parser[InferrableTerm] =
    (maybeApplication ~ opt("::" ~> typ) ^^ {
      case term ~ Some(typ) => Annotated(term, typ)
      case term ~ None => term
    }) | (lambdaTerm ~ ("::" ~> typ) ^^ {
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

  lazy val lambdaArgs: Parser[List[String]] = ???

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

  def typ: Parser[Type] =
    repsep(simpleType, "->") ^^
      (_.reduceRight[Type] { case (functionType, argumentType) => FunctionType(functionType, argumentType) })

  def simpleType: Parser[Type] = freeType | "(" ~> typ <~ ")"

  def freeType: Parser[FreeType] = ident ^^ (name => FreeType(name))

}
