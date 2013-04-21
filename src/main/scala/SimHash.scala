
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
  def rotateLeft(i: Int) = new HashValue(Integer.rotateLeft(hash, i))
  def permutation(i: Int) = rotateLeft(i)
  def permutations = for (i <- 0 to 32) yield permutation(i)
  def compare(that: HashValue) = hash.compare(that.hash)
  def distance(that: HashValue) = bitDiff(hash,that.hash)
  override def toString = "%32s".format(hash.toBinaryString).replace(' ', '0') 
  def toInt = hash
  def bitDiff(a:Int, b: Int) = bitCount(~(a ^ b))
  def bitCount(board:Int): Int = {
    def bitIsSet(bit:Int) : Boolean = {
      val mask = (1 << bit)
      (board & mask) == mask
    }
    (0 until 32).filter(bitIsSet).length
  }
}

object File {
  def apply(filename: String) = new File(filename, File.hashValue(filename)) 
  def tokens(filename: String): Seq[String] = tokenize(open(filename).mkString(""))
  def tokenize(text: String): Seq[String] = text.toLowerCase.split("""\s+""")
  def open(file: String) = Source.fromURL(getClass.getResource(file))
  def hashValue(filename: String) = HashValue(tokens(filename).map(word => word.hashCode))
}

case class File(val filename: String, val hash: HashValue) extends Ordered[File] {
  def permutation(i: Int) = new File(filename, hash.permutation(i))
  def tokens: Seq[String] = File.tokens(filename)
  def compare(that: File) = hash.compare(that.hash)
  def distance(that: File) = hash.distance(that.hash)
  override def toString = "%s (%d): %s, %s".format(hash, hash.toInt, filename, tokens.take(20).mkString(" "))
}

object HashSim {

  def main(args: Array[String]) = {
    val files: Seq[File] = fileList.map(file => File(file))
    for (i <- 0 until 32) {
      val order = files.map(f => f.permutation(i)).sorted
      order.foldLeft(new HashValue(0)){
        (hash, file) => 
          println("File " + file.filename + " distance " + file.hash.distance(hash).toString)
          file.hash
      }
    }
  }

  def fileList: Seq[String] = open("files.txt").getLines.toList
  def open(file: String) = Source.fromURL(getClass.getResource(file))
}
