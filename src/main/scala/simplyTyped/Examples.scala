package simplyTyped

import scala.language.implicitConversions

object Examples {
  import Term._
  val id: CheckableTerm = Lambda(Inf(BoundVariable(0)))
  val const: CheckableTerm = Lambda(Lambda(Inf(BoundVariable(1))))

  def free(name: String) = Inf(FreeVariable(name))

  // (id : a -> a) y
  val term1: InferrableTerm = (id :: (FreeType("a") -> FreeType("a"))) (free("y"))
  val term1Alt = Parser.parse("""((\x -> x) :: a -> a) y""")
  println(PrettyPrinter.prettyPrint(term1))
  // (const :: (b -> b) -> a -> (b -> b)) id y
  val term2: InferrableTerm = (const :: (FreeType("b") -> FreeType("b")) -> (FreeType("a") -> (FreeType("b") -> FreeType("b")))) (id)(free("y"))
  val term2Alt = Parser.parse("""((\x y -> x) :: (b -> b) -> a -> (b -> b)) (\x -> x) y""")
  println(PrettyPrinter.prettyPrint(term2))

  val Γ1 = Context.empty
    .withGlobalKind("a", *)
    .withGlobalType("y", FreeType("a"))

  val Γ2 = Γ1.withGlobalKind("b", *)

  // y
  val result1 = Evaluator.eval(term1)
  println(result1)
  val quotedResult1 = Quoter.quote(result1)
  println(quotedResult1)

  // a
  val Right(type1: Type) = TypeChecker.inferType(term1, Γ1)
  println(type1)

  val result2 = Evaluator.eval(term2)
  println(result2)
  val quotedResult2 = Quoter.quote(result2)

  println(quotedResult2)

  // b -> b
  val Right(type2: Type) = TypeChecker.inferType(term2, Γ2)
  println(type2)

  {
    // Int : *, String : *, n : Int
    val Γ = Context.empty
      .withGlobalKind("Int", *)
      .withGlobalKind("String", *)
      .withGlobalType("n", FreeType("Int"))
    // n : String
    val term = Annotated(Inf(FreeVariable("n")), FreeType("String"))
    println(TypeChecker.inferType(term, Γ))
  }


  {
    val Γ = Context.empty
      .withGlobalKind("Int", *)
      .withGlobalKind("String", *)
      .withGlobalType("s", FreeType("String"))

    val term = Lambda(BoundVariable(0)(FreeVariable("s"))) :: (FreeType("String") -> FreeType("Int")) -> FreeType("Int")
    println(TypeChecker.inferType(term, Γ))
  }

  println("Parsing:")
  println(Parser.parseAll(Parser.term, "f (x y) :: String -> Int -> Int"))
  println(Parser.parseAll(Parser.term, """(\x -> f x) :: String -> Int"""))
  println(Parser.parseAll(Parser.term, """(\x -> f x (\y -> x y z)) :: String -> Int"""))
  println(Parser.parseAll(Parser.term, """(\x -> x) :: String -> Int"""))
  println(Parser.parseAll(Parser.term, """(\x y z -> x y (\q -> x y)) :: String -> Int"""))

}
