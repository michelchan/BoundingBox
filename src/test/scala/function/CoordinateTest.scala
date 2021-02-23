package function

import org.scalatest.funsuite.AnyFunSuite


class CoordinateTest extends AnyFunSuite {

  /*** AreAdjacent ****/
  test("(1,1) and (1,2) are adjacent") {
    assert(checkAreAdjacent(Coordinate(1,1), Coordinate(1,2)))
  }

  test("(1,1) and (1,3) are not adjacent") {
    assert(!checkAreAdjacent(Coordinate(1,1), Coordinate(1,3)))
  }

  test("(2,5) and (6,10) are adjacent") {
    assert(checkAreAdjacent(Coordinate(2,5), Coordinate(6,10)))
  }

  test("(6,10) and (2,5) are adjacent") {
    assert(checkAreAdjacent(Coordinate(6,10), Coordinate(2,5)))
  }

  test("(1,5) and (3,3) are not adjacent") {
    assert(!checkAreAdjacent(Coordinate(1,5), Coordinate(3,3)))
  }

  /*** areIntersecting ***/
  test("(1,3) and (2,2) are intersecting") {
    assert(checkAreIntersecting(Coordinate(1,3), Coordinate(2,2)))
  }

  test("(2,2) and (2,4) are intersecting") {
    assert(checkAreIntersecting(Coordinate(2,2), Coordinate(2,4)))
  }

  test("(5,7) and (4,8) are intersecting") {
    assert(checkAreIntersecting(Coordinate(5,7), Coordinate(4,8)))
  }

  test("(1,3) and (4,8) are not intersecting") {
    assert(!checkAreIntersecting(Coordinate(1,3), Coordinate(4,8)))
  }

  test("(6,6) and (4,4) are not intersecting") {
    assert(!checkAreIntersecting(Coordinate(6,6), Coordinate(4,4)))
  }

  /*** tupleIntersection ***/
  test("(1,3) and (2,5) intersect into (1,5)") {
    assert(checkTupleCombine(Coordinate(1,3), Coordinate(2,5), Some(Coordinate(1,5))))
  }

  test("(2,5) and (1,3) intersect into (1,5)") {
    assert(checkTupleCombine(Coordinate(2,5), Coordinate(1,3), Some(Coordinate(1,5))))
  }

  test("(1,2) and (2,3) intersect into (1,3)") {
    assert(checkTupleCombine(Coordinate(1,2), Coordinate(2,3), Some(Coordinate(1,3))))
  }

  test("(1,1) and (1,1) intersect into (1,1)") {
    assert(checkTupleCombine(Coordinate(1,1), Coordinate(1,1), Some(Coordinate(1,1))))
  }

  test("(4,4) and (2,3) intersect into (2,4)") {
    assert(checkTupleCombine(Coordinate(4,4), Coordinate(2,3), Some(Coordinate(2,4))))
  }

  test("There are no intersections between (1,2) and (4,5)") {
    assert(checkTupleCombine(Coordinate(1,2), Coordinate(4,5), None))
  }

  test("There are no intersections between (4,4) and (1,2)") {
    assert(checkTupleCombine(Coordinate(4,4), Coordinate(1,2), None))
  }


  private def checkAreAdjacent(input1: Coordinate, input2: Coordinate): Boolean =
    input1.areAdjacent(input2)

  private def checkTupleCombine(input1: Coordinate, input2: Coordinate, expected: Option[Coordinate]): Boolean =
    input1.tupleCombine(input2).equals(expected)

  private def checkPointIntersection(input1: Coordinate, input2: Int): Boolean =
    input1.pointIntersection(input2)

  private def checkAreIntersecting(input1: Coordinate, input2: Coordinate): Boolean =
    input1.areIntersecting(input2)
}
