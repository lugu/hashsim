
import scala.io.Source

case class File(val filename: String) {
  lazy val tokens: Seq[String] = tokenize(open(filename).mkString(""))
  def tokenize(text: String): Seq[String] = text.toLowerCase.split("""\s+""")
  def open(file: String) = Source.fromURL(getClass.getResource(file))
  override def toString = filename + ": " + tokens.take(20).mkString(" ")
}

object HashSim {
  lazy val files: Seq[File] = fileList.map(file => new File(file))

  def main(args: Array[String]) = {
    files.foreach(println)
  }

  def fileList: Seq[String] = open("files.txt").getLines.toList
  def open(file: String) = Source.fromURL(getClass.getResource(file))
}

