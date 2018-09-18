package timeseries

case class Line(date: String, number: Int)

object Line {
  def parse(line: String): Line = {
    line.split(":") match {
      case Array(date, n) => Line(date, n.toInt)
      case _ => throw new IllegalArgumentException("Bad line format.")
    }
  }
  def serialize(line: Line): String = {
    line.date + ":" + line.number
  }

  def compareByDate(l1: Line, l2: Line): Boolean = l1.date == l2.date

  def mergeNumbers(l1: Line, l2: Line): Line = l1.copy(number = l1.number + l2.number)
}
