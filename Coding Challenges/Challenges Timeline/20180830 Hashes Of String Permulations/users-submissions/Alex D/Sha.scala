import java.security.MessageDigest

import scala.util.Random

object Sha {

  def sha1(string: String): String = {
    val sha1Digest = MessageDigest.getInstance("SHA1")
    sha1Digest.reset()
    val bytes = sha1Digest.digest(string.getBytes)
    bytes.map("%02X".format(_)).mkString
  }

  class PermutationIterator(values: Array[String]) extends Iterator[Seq[String]] {
    val hardcodedPermutations: Map[Int, Seq[Seq[Int]]] = Map(
      0 -> Nil,
      1 -> Seq(Seq(0)),
      2 -> Seq(Seq(0, 1), Seq(1, 0)),
      3 -> Seq(Seq(0, 1, 2), Seq(0, 2, 1), Seq(1, 0, 2), Seq(1, 2, 0), Seq(2, 0, 1), Seq(2, 1, 0))
    )

    var currentPermutation = 0
    override def hasNext: Boolean = hardcodedPermutations.get(values.length).exists(_.isDefinedAt(currentPermutation))
    override def next(): Seq[String] = {
      val result: Seq[String] = hardcodedPermutations(values.length)(currentPermutation).map(values.apply)
      currentPermutation += 1
      result
    }
  }

  val emojis: Map[Int, String] = Map(
    0 -> "\uD83D\uDE0E", // sunglasses
    1 -> "\uD83C\uDF40", // 4 leaf clover
    2 -> "✌️"  // victory
  )

  class RandomStringIterator(threadId: Int, length: Int) extends Iterator[Array[String]] {
    val random = new Random(threadId + 1234567890)
    var continue = true
    def emoji(idx: Int): String = emojis.getOrElse(idx, "")
    def stop(): Unit = continue = false
    override def hasNext: Boolean = continue
    override def next(): Array[String] = {
      (0 until length).toArray.map(idx => emoji(idx) + random.nextString(1))
    }
  }

  class SearchThread(threadId: Int) extends Thread {

    val sha1Digest = MessageDigest.getInstance("SHA1")
    def hexFormat(byte: Byte) = "%02X".format(byte)

    def sha1EndsBy0(string: String): Boolean = {
      sha1Digest.reset()
      val bytes = sha1Digest.digest(string.getBytes)
      bytes.lastOption.contains(0)
    }
    val randomStrings = new RandomStringIterator(threadId, 3)

    def prepareToStop(): Unit = randomStrings.stop()

    override def run(): Unit = {
      super.run()
      var count = 0
      randomStrings.foreach { strings =>
        count += 1
        val permutations = new PermutationIterator(strings)
        val matches = permutations.map(_.mkString).forall(value => sha1EndsBy0(value))
        if (matches) {
          println(s"Thread #$threadId, iteration #$count $matches: ${strings.mkString("\"", "\", \"", "\"")}")
        } else if (count % 1000000 == 0){
          println(s"Thread #$threadId, tested $count random sets of strings")
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("Press enter to stop")
    val threads = (0 until 4).map(new SearchThread(_))
    threads.foreach(_.start)
    var keyStroke = System.in.read()
    while (keyStroke != 10) {
      keyStroke = System.in.read()
    }
    threads.foreach(_.prepareToStop())
  }

  // testing
  def main2(args: Array[String]): Unit = {
    //val strings = IndexedSeq("颅㱹ℛ", "ĸ")
    //val strings = Array("릲蓎", "年돡")
    val strings = Array("😎ᘨ", "🍀㕖")
    new PermutationIterator(strings).map(_.mkString).foreach { permutation =>
      println(s"permutation: $permutation sha1: ${sha1(permutation)}")
    }
  }
}
