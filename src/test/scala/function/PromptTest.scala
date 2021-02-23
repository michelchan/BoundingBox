package function

import org.scalatest.funsuite.AnyFunSuite

class PromptTest extends AnyFunSuite{

  test("box1 will return (2,2)(3,3)") {
    assert(Box.getNonOverlappedBoxes(Utils.readFile("src/test/resources/box1")) == "(2,2)(3,3)\n")
  }

  test("box2 will return (1,1)(2,2)") {
    assert(Box.getNonOverlappedBoxes(Utils.readFile("src/test/resources/box2")) == "(1,1)(2,2)\n")
  }

  test("box3 will return (1,9)(3,12)\n(2,1)(3,1)") {
    assert(Box.getNonOverlappedBoxes(Utils.readFile("src/test/resources/box3")) == "(1,9)(3,12)\n(2,1)(3,1)\n")
  }
}
