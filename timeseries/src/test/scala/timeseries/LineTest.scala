package timeseries

import org.scalatest.{FunSuite, Matchers}

class LineTest extends FunSuite with Matchers {
  test("Parsing line") {
    Line.parse("1988-04-08:1") shouldEqual Line("1988-04-08", 1)
    intercept[IllegalArgumentException]{
      Line.parse("wrong input")
    }
  }

  test("Serializing line") {
    Line.serialize(Line("1988-04-08", 1)) shouldEqual "1988-04-08:1"
  }
}
