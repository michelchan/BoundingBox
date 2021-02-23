package function


import function.Box.{ColumnCoordinate, RowCoordinate}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex


final case class Box(rowCoordinate: RowCoordinate, columnCoordinate: ColumnCoordinate) { self =>

  def combineBoxes(box: Box): Option[Box] = {
    val newRow = self.rowCoordinate.tupleCombine(box.rowCoordinate)
    val newColumn = self.columnCoordinate.tupleCombine(box.columnCoordinate)

    if (!(newRow.isDefined && newColumn.isDefined)) None
    else Some(Box(newRow.get, newColumn.get))
  }

  def needsCombining(box: Box): Boolean = {
    self.rowCoordinate.needsCombining(box.rowCoordinate) && self.columnCoordinate.needsCombining(box.columnCoordinate)
  }

  def areIntersecting(box: Box): Boolean = {
    self.rowCoordinate.areIntersecting(box.rowCoordinate) && self.columnCoordinate.areIntersecting(box.columnCoordinate)
  }

  def compareWith(box: Box): Boolean = {
    val col1 = self.columnCoordinate
    val col2 = box.columnCoordinate
    val row1 = self.rowCoordinate
    val row2 = box.rowCoordinate

    val colResult = col1.compareWith(col2)
    val rowResult = row1.compareWith(row2)

    if (colResult == -1) true
    else if (colResult == 1) false
    else rowResult <= 0
  }
}


object Box {

  def getNonOverlappedBoxes(input: String): String = {
    getNonOverlappedBoxes(Utils.strListToMultiCharArray(input.split("\n").toList))
  }

  def getNonOverlappedBoxes(input: Array[Array[Char]]) = {
    val rowCount = input.length
    val columnCount = input.head.length
    val queue: mutable.Queue[Box] = mutable.Queue()

    for (row <- 0 until rowCount){
      for (column <- 0 until columnCount) {
        input(row)(column) match {
          case '*' => {
            queue.enqueue(Box(Coordinate((row,row)), Coordinate((column, column))))
          }
          case _ =>
        }
      }
    }

    val groupedBoxes=queue.toList.foldLeft(List[Box]())((list, box) => {
      groupBoxes(box, list)
    })

    formatResults(removeOverlap(groupedBoxes))
  }

  def formatResults(boxes: List[Box]): String = {
    def incrementTuple(t: (Int, Int)): (Int,Int) = {
      (t._1+1, t._2+1)
    }

    boxes.flatMap(box => {
      val row = incrementTuple(box.rowCoordinate.range)
      val column = incrementTuple(box.columnCoordinate.range)

      s"(${row._1},${column._1})(${row._2},${column._2})\n"
    }).mkString
  }

  def groupBoxes(box: Box, boxList: List[Box]): List[Box] = {
    val needCombine = boxList.filter(b1 => box.needsCombining(b1))
    if (boxList.isEmpty || needCombine.isEmpty) box :: boxList
    else {
      val unFiltered = boxList.filterNot(b1 => box.needsCombining(b1))

      needCombine.map(b1 => {
        b1.combineBoxes(box).get
      }) ::: unFiltered
    }
  }

  def removeOverlap(boxList: List[Box]): List[Box] = {
    var result = new ListBuffer[Box]()
    var overlapped = new mutable.HashSet[Box]() // using var and mutable for performance

    boxList.zipWithIndex.map{ case (box, index) => {
      val slicedList = boxList.drop(index+1)
      val overlappingBoxes = slicedList.filter(b1 =>
        box.areIntersecting(b1) && !overlapped.contains(b1))
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

  def getRowBoxes(row: String, rowNum: Int): List[Box] = {
    val starPattern: Regex = """\*+""".r

    starPattern.findAllMatchIn(row).map{
      index =>
        Box(Coordinate((rowNum, rowNum)), Coordinate((index.start, index.end-1)))
    }.toList
  }

  type ColumnCoordinate = Coordinate
  type RowCoordinate = Coordinate
}