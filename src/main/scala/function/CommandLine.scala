package function

import scala.io.StdIn

object CommandLine {

  /***
   * Gets the nonoverlapping bounding boxes from input
   * Either get file path from args (preferred)
   * or new line separated input read from stdin (stdin will stop reading when you type "end")
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val result = args.headOption match {
      case Some(path) => Box.getNonOverlappedBoxes(Utils.readFile(path))
      case None => {
        val input = Iterator.
          continually(StdIn.readLine).
          takeWhile(_ != "end").
          mkString("\n")

        Box.getNonOverlappedBoxes(input)
      }
    }
    println(result)
  }
}
