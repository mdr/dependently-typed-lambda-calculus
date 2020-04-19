package simplyTyped

object Examples extends scala.App {

  val id: CheckableTerm = Lam(Inf(Bound(0)))
  val const: CheckableTerm = Lam(Lam(Inf(Bound(1))))

  def free(name: String) = Inf(Free(name))

  // (id : a -> a) y
  val term1: InferrableTerm = (id :: (FreeType("a") -> FreeType("a"))) (free("y"))

  // (const :: (b -> b) -> a -> (b -> b)) id y
  val term2: InferrableTerm = (const :: (FreeType("b") -> FreeType("b")) -> (FreeType("a") -> (FreeType("b") -> FreeType("b")))) (id)(free("y"))

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
    val term = Ann(Inf(Free("n")), FreeType("String"))
    println(TypeChecker.inferType(term, Γ))
  }

}
