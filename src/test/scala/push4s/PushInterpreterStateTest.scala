package push4s

import org.scalatest.{FunSpec, Matchers}

class PushInterpreterStateTest extends FunSpec with Matchers {
  describe("PushInterpreterState") {
    it ("should be able to start a program") {
      val program = List[PushElement](PushInt(3))

      val state = PushInterpreterState.fromProgram(program)
      state.exec.contents shouldBe program
      state.int.contents shouldBe empty
      state.boolean.contents shouldBe empty
      state.string.contents shouldBe empty
      state.code.contents shouldBe empty

    }
  }

}
