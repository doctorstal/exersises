package timeseries

import java.io.{File, PrintWriter}

import scala.io.Source
import IteratorExtensions._

object TimeSeriesMerge extends App {

  args match {
    case Array(inputDir, outputPath) =>
      mergeFromDirToFile(inputDir, outputPath)
    case _ => println("You should pass two parameters: source dir and output path.\n Example: sbt \"run ./data output.txt\"")
  }

  def mergeFromDirToFile(inputDir: String, outputFile: String): Unit = {
    val lineIterators = lineIteratorsInDir(inputDir)
    val writer = new PrintWriter(outputFile)

    IteratorExtensions.minValueIterator[Line, String](lineIterators, _.date)
      .spanAndFold(Line.compareByDate, Line.mergeNumbers)
      .foreach {
        line => writer.println(Line.serialize(line))
      }

    writer.close()
  }

  private def lineIteratorsInDir(inputDir: String): Seq[Iterator[Line]] = {
    filesInDir(inputDir).map { file =>
      Source.fromFile(file)
        .getLines
        .map(Line.parse)
    }
  }

  def filesInDir(path: String): Seq[File] = {
    val dir = new File(path)
    if (dir.exists && dir.isDirectory) {
      dir.listFiles.filter(_.isFile).toSeq
    } else {
      List()
    }
  }
}
