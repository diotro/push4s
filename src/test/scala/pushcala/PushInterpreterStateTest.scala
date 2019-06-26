package pushcala

import org.scalatest.{FunSpec, Matchers}

class PushInterpreterStateTest extends FunSpec with Matchers {
  describe("PushInterpreterState") {
    it ("should be able to start a program") {
      val program = List[PushAtom](LiteralInt(3))

      val state = PushInterpreterState.startProgram(program)
      state.exec.contents shouldBe program
      state.int.contents shouldBe empty
      state.boolean.contents shouldBe empty
      state.string.contents shouldBe empty
      state.code.contents shouldBe empty

    }
  }

}
