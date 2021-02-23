package function

import org.scalatest.funsuite.AnyFunSuite

class BoxTest  extends AnyFunSuite{

    /*** getRowBoxes ***/
    test("'-' has no boxes and should return None") {
      assert(checkBoxResults("-", List()))
    }

    test("'-***-' has a box from columns 1-3") {
      assert(checkBoxResults("-***-", List(Box(Coordinate(1,1), Coordinate(1,3)))))
    }

    test("'-*--**-' has boxes from columns 1-1 and 4-5") {
      assert(checkBoxResults("-*--**-", List(Box(Coordinate(1,1), Coordinate(1,1)), Box(Coordinate(1,1),Coordinate(4,5)))))
    }

    /*** compareWith ***/
    test("Box((1,1), (4,4)) is sorted less than Box((2,2),(4,4))") {
      assert(checkCompareWith(Box(Coordinate(1,1), Coordinate(4,4)),
        Box(Coordinate(2,2), Coordinate(4,4))))
    }

    test("Box((1,1), (2,2)) is sorted less than Box((1,1),(2,3))") {
      assert(checkCompareWith(Box(Coordinate(1,1), Coordinate(2,2)),
        Box(Coordinate(1,1), Coordinate(2,3))))
    }

    test("Box((1,1), (2,2)) is sorted less than Box((1,2),(2,2))") {
      assert(checkCompareWith(Box(Coordinate(1,1), Coordinate(2,2)),
        Box(Coordinate(1,2), Coordinate(2,2))))
    }

    test("Box((1,2), (2,2)) is sorted greater than Box((1,1),(2,2))") {
      assert(!checkCompareWith(Box(Coordinate(1,2), Coordinate(2,2)),
        Box(Coordinate(1,1), Coordinate(2,2))))
    }

    test("Box((1,1), (1,1)) is sorted less than Box((1,1),(1,1))") {
      assert(checkCompareWith(Box(Coordinate(1,1), Coordinate(1,1)),
        Box(Coordinate(1,1), Coordinate(1,1))))
    }

    test("Box((1,1), (1,2)) is sorted greater than Box((1,1),(1,1))") {
      assert(!checkCompareWith(Box(Coordinate(1,1), Coordinate(1,2)),
        Box(Coordinate(1,1), Coordinate(1,1))))
    }

    test("Box((3,1), (1,1)) is sorted greater than Box((1,1),(1,1))") {
      assert(!checkCompareWith(Box(Coordinate(3,1), Coordinate(1,1)),
        Box(Coordinate(1,1), Coordinate(1,1))))
    }

  /*** combineBoxes ***/
    test("Box((1,1), (1,1)) should combine with Box((1,2), (1,2)) to get (1,2)(1,2)") {
      assert(checkCombineBoxes(Box(Coordinate(1,1), Coordinate(1,1)),
        Box(Coordinate(1,2), Coordinate(1,2)),
        Some(Box(Coordinate(1,2), Coordinate(1,2)))
      ))
    }

    test("Box((1,1), (1,1)) does not combine with Box((5,5), (5,5))") {
      assert(checkCombineBoxes(Box(Coordinate(1,1), Coordinate(1,1)),
        Box(Coordinate(5,5), Coordinate(5,5)),
        None
      ))
    }

    test("Box((1,3), (1,3)) should combine with Box((2,2), (2,2)) to get (1,3)(1,3)") {
      assert(checkCombineBoxes(Box(Coordinate(1,3), Coordinate(1,3)),
        Box(Coordinate(2,2), Coordinate(2,2)),
        Some(Box(Coordinate(1,3), Coordinate(1,3)))
      ))
    }

  /*** needsCombining ***/
    test("Box((1,1), (1,1)) should combine with Box((1,2), (1,2))") {
      assert(checkNeedsCombining(Box(Coordinate(1,1), Coordinate(1,1)),
        Box(Coordinate(1,2), Coordinate(1,2)),
      ))
    }

    test("Box((1,1), (1,1)) should not combine with Box((1,1), (5,9))") {
      assert(!checkNeedsCombining(Box(Coordinate(1,1), Coordinate(1,1)),
        Box(Coordinate(1,1), Coordinate(5,9)),
      ))
    }

    test("Box((1,3), (1,3)) should combine with Box((2,2), (2,2))") {
      assert(checkNeedsCombining(Box(Coordinate(1,3), Coordinate(1,3)),
        Box(Coordinate(2,2), Coordinate(2,2)),
      ))
    }

  /*** areIntersecting ***/
    test("Box((1,1), (1,1)) intersects Box((1,2), (1,2))") {
      assert(checkAreIntersecting(Box(Coordinate(1,1), Coordinate(1,1)),
        Box(Coordinate(1,2), Coordinate(1,2)),
      ))
    }

    test("Box((1,1), (1,1)) does not intersect Box((2,2), (2,2))") {
      assert(!checkAreIntersecting(Box(Coordinate(1,1), Coordinate(1,1)),
        Box(Coordinate(2,2), Coordinate(2,2)),
      ))
    }

    test("Box((1,3), (1,3)) intersects Box((2,2), (2,2))") {
      assert(checkAreIntersecting(Box(Coordinate(1,3), Coordinate(1,3)),
        Box(Coordinate(2,2), Coordinate(2,2)),
      ))
    }

    test("Box(1,1), (3,3) prints to '(1,3)(1,3)'") {
      assert(Box.formatResults(List(Box(Coordinate(1,1), Coordinate(3,3)))) == "(2,4)(2,4)\n")
    }

    test("List[Box(1,3)(1,3), Box(2,2)(2,2)] overlaps") {
      assert(Box.removeOverlap(List(Box(Coordinate(1,3), Coordinate(1,3)),
        Box(Coordinate(2,2), Coordinate(2,2))
      )).isEmpty)
    }

    test("List[Box(1,1)(3,3), Box(2,2)(2,2)] has no overlaps") {
      assert(Box.removeOverlap(List(Box(Coordinate(1,1), Coordinate(3,3)),
        Box(Coordinate(2,2), Coordinate(2,2))
      )).size == 2)
    }

    test("Box(1,1)(1,1) groups with List[Box(1,2)(1,2)]") {
      assert(Box.groupBoxes(Box(Coordinate(1,1), Coordinate(1,1)),
        List(Box(Coordinate(1,2), Coordinate(1,2))
      )).size == 1)
    }

    test("Box(1,1)(1,1) does not group with List[Box(1,1)(3,4)]") {
      assert(Box.groupBoxes(Box(Coordinate(1,1), Coordinate(1,1)),
        List(Box(Coordinate(1,1), Coordinate(3,4))
      )).size == 2)
    }


    private def checkBoxResults(string: String, box: List[Box]): Boolean =
      Box.getRowBoxes(string, 1).equals(box)

    private def checkCompareWith(box1: Box, box2: Box): Boolean =
      box1.compareWith(box2)

    private def checkCombineBoxes(input1: Box, input2: Box, expected: Option[Box]): Boolean =
      input1.combineBoxes(input2).equals(expected)

    private def checkNeedsCombining(input1: Box, input2: Box): Boolean =
      input1.needsCombining(input2)

    private def checkAreIntersecting(input1: Box, input2: Box): Boolean =
      input1.areIntersecting(input2)
}
