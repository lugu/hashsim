
import scala.io.Source

object HashValue {
  def apply(codes: Seq[Int]): HashValue = new HashValue(value(hashVector(codes)))
  def hashVector(codes: Seq[Int]) = hashCounts(codes).map(c => if (c <= 0) 0 else 1)

  def hashCounts(codes: Seq[Int]) = {
    for (i <- 0 until 32) yield codes.map(c => emitBit(i, c)).reduce(_+_)
  }
  def bit(i: Int, code: Int) = (code>>i)&1
  def emitBit(i: Int, code: Int): Int = if (bit(i, code) == 1) 1 else -1
  def value(hash: Seq[Int]): Int = (for (i <- 0 until 32) yield hash(i) << i).reduce(_+_)
}

case class HashValue(val hash: Int) extends Ordered[HashValue] {
  def hashes = for (i <- 0 until 32) yield new HashValue(Integer.rotateLeft(hash, i))
  def toInt = hash
  override def toString = (for (i <- 0 until 32) yield HashValue.bit(i, hash)).mkString("")
  def compare(that: HashValue): Int = hash - that.hash
}

case class File(val filename: String) extends Ordered[File] {

  val hash = HashValue(tokens.map(word => word.hashCode))

  def hashes: Seq[HashValue] = hash.hashes
  def compare(that: File): Int = hash.compare(that.hash)
  def tokens: Seq[String] = tokenize(open(filename).mkString(""))
  def tokenize(text: String): Seq[String] = text.toLowerCase.split("""\s+""")
  def open(file: String) = Source.fromURL(getClass.getResource(file))
  override def toString = "%s (%d): %s, %s".format(hash, hash.toInt, filename, tokens.take(20).mkString(" "))
}

object HashSim {
  lazy val files: Seq[File] = fileList.map(file => new File(file))

  def main(args: Array[String]) = {
    files.foreach(println)
  }

  def fileList: Seq[String] = open("files.txt").getLines.toList
  def open(file: String) = Source.fromURL(getClass.getResource(file))
}

