package function

object CommandLine {

  def main(args: Array[String]): Unit = {
    val x = Box.getNonOverlappedBoxes(Utils.readFile("src/test/resources/box3"))
    println(x)
  }
}
