package function

import scala.io.Source

object Utils {

  def readFile(fileName: String): Array[Array[Char]] = {
    Source.fromFile(fileName).getLines().map(s => s.toCharArray).toArray
  }


}
