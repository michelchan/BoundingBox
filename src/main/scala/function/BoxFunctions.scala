package function

import function.Box.{ColumnCoordinate, RowCoordinate}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex


case class Box(rowCoordinate: RowCoordinate, columnCoordinate: ColumnCoordinate)


object Box {

  def getNonOverlappedBoxes(input: Array[Array[Char]]) = {
    val rowCount = input.length
    val columnCount = input.head.length
    val queue: mutable.Queue[Box] = mutable.Queue()

    for (row <- 0 until rowCount){
      for (column <- 0 until columnCount) {
        input(row)(column) match {
          case '*' => {
            queue.enqueue(Box((row, row), (column, column)))
          }
          case _ =>
        }
      }
    }

    val groupedBoxes = groupTheBoxes(queue.toList, List())
    formatResults(removeOverlap(groupedBoxes))
  }

  def formatResults(boxes: List[Box]) = {
    def incrementTuple(t: (Int, Int)): (Int,Int) = {
      (t._1+1, t._2+1)
    }

    boxes.flatMap(box => {
      s"${incrementTuple(box.rowCoordinate)} ${incrementTuple(box.columnCoordinate)}\n"
    }).mkString
  }

  def groupTheBoxes(queue: List[Box], result: List[Box]): List[Box] = {
    if (queue.isEmpty) result
    else {
      val box = queue.head
      groupTheBoxes(queue.drop(1), groupBoxes(box, result))
    }
  }

  def groupBoxes(box: Box, boxList: List[Box]): List[Box] = {
    val needCombine = boxList.filter(b1 => needsCombining(box, b1))
    if (boxList.isEmpty || needCombine.isEmpty) box :: boxList
    else {
      val unFiltered = boxList.filterNot(b1 => needsCombining(box, b1))

      needCombine.map(b1 => {
        combineBoxes(b1, box).get
      }) ::: unFiltered
    }
  }

  def removeOverlap(boxList: List[Box]): List[Box] = {
    var result = new ListBuffer[Box]()
    var overlapped = new mutable.HashSet[Box]()

    boxList.zipWithIndex.map{ case (box, index) => {
      val slicedList = boxList.drop(index+1)
      val overlappingBoxes = slicedList.filter(b1 =>
        areIntersecting(b1.columnCoordinate, box.columnCoordinate) && areIntersecting(b1.rowCoordinate, box.rowCoordinate) && !overlapped.contains(b1))
      if (overlappingBoxes.isEmpty && !overlapped.contains(box)) {
        result += box
      }
      else {
        overlapped += box
        overlapped ++= overlappingBoxes
      }
    }}

    result.toList
  }

  def combineBoxes(b1: Box, b2: Box): Option[Box] = {
    val newRow = tupleCombine(b1.rowCoordinate, b2.rowCoordinate)
    val newColumn = tupleCombine(b1.columnCoordinate, b2.columnCoordinate)

    if (!(newRow.isDefined && newColumn.isDefined)) None
    else Some(Box(newRow.get, newColumn.get))
  }

  def needsCombining(b1: Box, b2: Box): Boolean = {
    needsCombining(b1.rowCoordinate, b2.rowCoordinate) && needsCombining(b1.columnCoordinate, b2.columnCoordinate)
  }

  def needsCombining(p1: (Int,Int), p2: (Int, Int)): Boolean = {
    areIntersecting(p1, p2) || areAdjacent(p1, p2)
  }

  def isAdjacent(t1: (Int,Int), t2: Int): Boolean = {
    (t1._1 - t2).abs == 1 || ((t1._2 - t2).abs == 0)
  }

  def areAdjacent(t1: (Int,Int), t2: (Int, Int)): Boolean = {
    !((t1._1 - t2._1).abs == 1 && (t1._2 - t2._2).abs == 1) && ((t1._1 - t2._1).abs == 1) || ((t1._2 - t2._1).abs == 1) || ((t2._2 - t1._1).abs == 1) || ((t1._2 - t2._2).abs == 1)
  }

  def pointIntersection(t1: (Int, Int), t2:Int): Boolean = {
    if (t1._1 <= t2 && t1._2 >= t2) true
    else false
  }

  def areIntersecting(t1: (Int, Int), t2: (Int, Int)): Boolean = {
    if (t1._1 <= t2._1 && t1._2 >= t2._1) true
    else if (t2._1 <= t1._1 && t2._2 >= t1._1) true
    else false
  }

  def tupleCombine(t1: (Int, Int), t2: (Int, Int)): Option[(Int,Int)] = {
    needsCombining(t1, t2) match {
      case true => Some(t1._1.min(t2._1),t1._2.max(t2._2))
      case false => None
    }
  }

  def compareWith(box1: Box, box2: Box): Boolean = {
    val col1 = box1.columnCoordinate
    val col2 = box2.columnCoordinate
    val row1 = box1.rowCoordinate
    val row2 = box2.rowCoordinate

    val colResult = compareWith(col1, col2)
    val rowResult = compareWith(row1, row2)

    if (colResult == -1) true
    else if (colResult == 1) false
    else rowResult <= 0
  }

  private def compareWith(t1: (Int, Int), t2: (Int, Int)): Int = {
    import scala.math.Ordering.Implicits._

    if (t1 < t2) -1
    else if (t1 == t2) 0
    else 1
  }

  def getRowBoxes(row: String, rowNum: Int): List[Box] = {
    val starPattern: Regex = """\*+""".r

    starPattern.findAllMatchIn(row).map{
      index =>
        Box((rowNum, rowNum), (index.start, index.end-1))
    }.toList
  }

  type ColumnCoordinate = (Int, Int)
  type RowCoordinate = (Int, Int)

}