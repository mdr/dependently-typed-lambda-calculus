package dependentlyTyped

case class NameSupplier(names: Seq[String] = "abcdefghijklmnopqrstuvwxyz".split("")) {
  def getName(avoid: Seq[String]): (String, NameSupplier) = {
    val availableNames = names.filterNot(avoid.contains)
    availableNames.head -> NameSupplier(availableNames.tail)
  }

  def getNames(n: Int, avoid: Seq[String]): (Seq[String], NameSupplier) =
    if (n == 0)
      (Seq.empty, this)
    else {
      val (names, nameSupplier2) = getNames(n - 1, avoid)
      val (name, nameSupplier3) = nameSupplier2.getName(avoid = avoid)
      (name +: names, nameSupplier3)
    }
}
