package timeseries

import scala.collection.AbstractIterator


object IteratorExtensions {

  implicit class IteratorExt[T](iterator: Iterator[T]) {
    def spanAndFold(spanPredicate: (T, T) => Boolean, foldOperator: (T, T) => T): Iterator[T] = new AbstractIterator[T] {
      var lines: Iterator[T] = iterator

      def hasNext: Boolean = lines.hasNext

      def next(): T = {
        val head = lines.next()
        iterator.span(spanPredicate(head, _)) match {
          case (combo, tail) =>
            lines = tail
            combo.fold(head)(foldOperator)
        }
      }
    }
  }

  def minValueIterator[A, B](base: Seq[Iterator[A]], minFn: A => B)
                            (implicit cmp: Ordering[B]): Iterator[A] = new AbstractIterator[A] {
    var iterators: Seq[BufferedIterator[A]] = base.collect {
      case x if x.hasNext => x.buffered
    }

    def hasNext: Boolean = iterators.nonEmpty

    def next: A = {
      val it = iterators.minBy(it => minFn(it.head))
      val res = it.next()
      if (it.isEmpty) {
        iterators = iterators.filter(_.hasNext)
      }
      res
    }
  }

}
