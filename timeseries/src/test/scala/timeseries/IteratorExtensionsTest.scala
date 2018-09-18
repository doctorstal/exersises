package timeseries

import org.scalatest.{FunSuite, Matchers}

class IteratorExtensionsTest extends FunSuite with Matchers {
  test("Min value iterator should select least value from bunch of iterators for each next value") {
    IteratorExtensions.minValueIterator(
      Seq(Iterator.range(0, 6), Iterator.range(2, 8), Iterator.range(3, 6)), identity[Int]
    ).toList shouldEqual List(
      0,
      1,
      2, 2,
      3, 3, 3,
      4, 4, 4,
      5, 5, 5,
         6,
         7
    )
  }

  test("Span and fold should iterate over folded group of elements") {
    import IteratorExtensions._
    Iterator("One", "Two", "Three", "Test", "Four", "Five", "Six", "Seven", "Eight")
      .spanAndFold((s1, s2) => s1(0) == s2(0), (s1, s2) => s1 + s2)
      .toList shouldEqual List(
      "One",
      "TwoThreeTest",
      "FourFive",
      "SixSeven",
      "Eight"
    )
  }
}
