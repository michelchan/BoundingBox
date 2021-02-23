package function

import scala.io.Source

object Utils {

  def readFile(fileName: String): Array[Array[Char]] = {
    strListToMultiCharArray(Source.fromFile(fileName).getLines().toList)
  }

  def strListToMultiCharArray(list: List[String]): Array[Array[Char]] = {
    list.map(s => s.toCharArray).toArray
  }
}
