package push4s

import org.scalatest.{FunSpec, FunSuite, Matchers}

class PushStackTest extends FunSpec with Matchers {
  describe("PushStack") {
    it ("should allow you to retrieve pushed items") {
      val (item, stack) = PushStack[Int](List()).push(3).pop()
      item should contain(3)
    }

    it("should pop the most recently pushed item") {
      val (item, stack) = PushStack[Int](List()).push(3).push(4).pop()
      item should contain(4)
    }

    it ("should remove popped items") {
      val (item, stack) = PushStack[Int](List()).push(3).push(4).pop()
      item should contain(4)
      val (item2, stack2) = stack.pop()
      item2 should contain(3)
      stack2.pop()._1 should be (None)
    }

    it ("should return None if popped with no values") {
      val (item, stack)= PushStack[Int](List()).pop()
      item shouldBe None
    }
  }
}
