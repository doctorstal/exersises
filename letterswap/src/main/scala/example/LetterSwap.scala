package example

import scala.annotation.tailrec
import scala.io.Source

object LetterSwap extends App {
  args match {
    case Array(word1, word2, vocabularyPath) =>
      findPath(word1.toUpperCase, word2.toUpperCase, vocabularyPath)
        .foreach(println)
    case _ => println("Please provide your word as first argument and vocabulary as second.")
  }

  def findPath(w1: String, w2: String, path: String): Seq[String] = {
    if (w1.length == w2.length) {
      initNodes(path, w1, w2) match {
        case (Some(n1), Some(n2)) =>
          findConnection(n1, n2)
        case _ => Seq.empty
      }
    } else {
      Seq.empty
    }
  }

  def initNodes(path: String, w1: String, w2: String) = {
    val dict = Source.fromFile(path)
      .getLines
      .collect {
        case value if value.length == w1.length => Node(value)
      }
      .toSet
    dict.foreach { n1 =>
      n1.connections = dict.filter(connected(n1, _))
    }
    (dict.find(_.value == w1), dict.find(_.value == w2))
  }


  def findConnection(n1: Node[String], n2: Node[String]): Seq[String] = {
    aStar[String](Set(n1), Set())(value => differentLetters(value, n2.value), n2)
      .map(node => node.nodesToFirst)
      .toSeq
      .flatten
      .map(_.value)
  }

  @tailrec
  def aStar[A](openSet: Set[Node[A]], closedSet: Set[Node[A]])
              (implicit heuristics: A => Int, end: Node[A]): Option[Node[A]] = {
    if (openSet.isEmpty) {
      None
    } else {
      val min = openSet.minBy(_.fScore)
      if (min == end) {
        Some(min)
      } else {
        min.connections
          .diff(closedSet)
          .foreach { n =>
            // This 1 makes no sense - we have same distance between connected nodes,
            // but it is here to show distance could be different
            val fScore = 1 + heuristics(n.value)
            if (fScore < n.fScore) {
              n.fScore = fScore
              n.prev = Some(min)
            }
          }
        aStar(openSet ++ min.connections - min, closedSet + min)
      }
    }
  }


  def connected(n1: Node[String], n2: Node[String]): Boolean = {
    differentLetters(n1.value, n2.value) == 1
  }


  def differentLetters(s1: String, s2: String): Int = {
    (s1 zip s2).count { case (c1, c2) => c1 != c2 }
  }
}

class Node[A](val value: A) {
  var connections: Set[Node[A]] = Set.empty[Node[A]]
  var prev: Option[Node[A]] = None
  var fScore: Int = Int.MaxValue

  def nodesToFirst: Seq[Node[A]] = prev.map(_.nodesToFirst).getOrElse(Seq()) :+ this
}

object Node {
  def apply[A](value: A): Node[A] = {
    new Node(value)
  }
}
