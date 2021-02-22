package function

import org.scalatest.funsuite.AnyFunSuite


class BoxFunctionsTest extends AnyFunSuite{

  test("'-' has no boxes and should return None") {
    assert(checkBoxResults("-", List()))
  }

  test("'-***-' has a box from columns 1-3") {
    assert(checkBoxResults("-***-", List(Box((1,1), (1,3)))))
  }

  test("'-*--**-' has boxes from columns 1-1 and 4-5") {
    assert(checkBoxResults("-*--**-", List(Box((1,1), (1,1)), Box((1,1),(4,5)))))
  }

  /*** AreAdjacent ****/
  test("(1,1) and (1,2) are adjacent") {
    assert(checkAreAdjacent((1,1), (1,2)))
  }

  test("(1,1) and (1,3) are not adjacent") {
    assert(!checkAreAdjacent((1,1), (1,3)))
  }

  test("(2,5) and (6,10) are adjacent") {
    assert(checkAreAdjacent((2,5), (6,10)))
  }

  test("(6,10) and (2,5) are adjacent") {
    assert(checkAreAdjacent((6,10), (2,5)))
  }

  test("(1,5) and (3,3) are not adjacent") {
    assert(!checkAreAdjacent((1,5), (3,3)))
  }

  /*** areIntersecting ***/
  test("(1,3) and (2,2) are intersecting") {
    assert(checkAreIntersecting(List((1,3),(2,2))))
  }

  test("(2,2) and (2,4) are intersecting") {
    assert(checkAreIntersecting(List((2,2),(2,4))))
  }

  test("(5,7) and (4,8) are intersecting") {
    assert(checkAreIntersecting(List((5,7),(4,8))))
  }

  test("(1,3) and (4,8) are not intersecting") {
    assert(!checkAreIntersecting(List((1,3),(4,8))))
  }

  test("(6,6) and (4,4) are not intersecting") {
    assert(!checkAreIntersecting(List((6,6),(4,4))))
  }

  /*** tupleIntersection ***/
  test("(1,3) and (2,5) intersect into (1,5)") {
    assert(checkTupleCombine(List((1,3), (2,5)), Some((1,5))))
  }

  test("(2,5) and (1,3) intersect into (1,5)") {
    assert(checkTupleCombine(List((2,5), (1,3)), Some((1,5))))
  }

  test("(1,2) and (2,3) intersect into (1,3)") {
    assert(checkTupleCombine(List((1,2), (2,3)), Some((1,3))))
  }

  test("(1,1) and (1,1) intersect into (1,1)") {
    assert(checkTupleCombine(List((1,1), (1,1)), Some((1,1))))
  }

  test("(4,4) and (2,3) intersect into (2,4)") {
    assert(checkTupleCombine(List((4,4), (2,3)), Some((2,4))))
  }

  test("There are no intersections between (1,2) and (4,5)") {
    assert(checkTupleCombine(List((1,2), (4,5)), None))
  }

  test("There are no intersections between (4,4) and (1,2)") {
    assert(checkTupleCombine(List((4,4), (1,2)), None))
  }

  /*** compareWith ***/
  test("Box((1,1), (4,4)) is sorted less than Box((2,2),(4,4))") {
    assert(checkCompareWith(Box((1,1),(4,4)), Box((2,2),(4,4))))
  }

  test("Box((1,1), (2,2)) is sorted less than Box((1,1),(2,3))") {
    assert(checkCompareWith(Box((1,1),(2,2)), Box((1,1),(2,3))))
  }

  test("Box((1,1), (2,2)) is sorted less than Box((1,2),(2,2))") {
    assert(checkCompareWith(Box((1,1),(2,2)), Box((1,2),(2,2))))
  }

  test("Box((1,2), (2,2)) is sorted greater than Box((1,1),(2,2))") {
    assert(!checkCompareWith(Box((1,2),(2,2)), Box((1,1),(2,2))))
  }

  test("Box((1,1), (1,1)) is sorted less than Box((1,1),(1,1))") {
    assert(checkCompareWith(Box((1,1),(1,1)), Box((1,1),(1,1))))
  }

  test("Box((1,1), (1,2)) is sorted greater than Box((1,1),(1,1))") {
    assert(!checkCompareWith(Box((1,1),(1,2)), Box((1,1),(1,1))))
  }

  test("Box((3,1), (1,1)) is sorted greater than Box((1,1),(1,1))") {
    assert(!checkCompareWith(Box((3,1),(1,1)), Box((1,1),(1,1))))
  }


  private def checkBoxResults(string: String, box: List[Box]): Boolean =
    Box.getRowBoxes(string, 1).equals(box)

  private def checkIsAdjacent(input1: (Int, Int), input2: Int): Boolean =
    Box.isAdjacent(input1, input2)

  private def checkAreAdjacent(input1: (Int, Int), input2: (Int, Int)): Boolean =
    Box.areAdjacent(input1, input2)

  private def checkTupleCombine(inputs: List[(Int, Int)], expected: Option[(Int,Int)]): Boolean =
    Box.tupleCombine(inputs.head, inputs.last).equals(expected)

  private def checkPointIntersection(input1: (Int, Int), input2: Int): Boolean =
    Box.pointIntersection(input1, input2)

  private def checkAreIntersecting(inputs: List[(Int, Int)]): Boolean =
    Box.areIntersecting(inputs.head, inputs.last)

  private def checkCompareWith(box1: Box, box2: Box): Boolean =
    Box.compareWith(box1, box2)

  private def checkAreAdjacent(inputs: List[(Int,Int)]): Boolean =
    Box.areAdjacent(inputs.head, inputs.last)

  private def checkCombineBoxes(inputs: List[Box]): Boolean =
    Box.combineBoxes()
}
