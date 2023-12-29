package chapter12

import munit.FunSuite

class BasicTest extends FunSuite {
  test("2 times 2 should always be 4") {
    assertEquals(2 * 2, 4)
  }
}
