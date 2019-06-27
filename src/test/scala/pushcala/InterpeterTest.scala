package pushcala

import org.scalatest.{FunSpec, Matchers}

class PushInterpeterTest extends FunSpec with Matchers {
  describe("PushInterpreter") {
    describe("run()") {
      it ("should return immediately if exec is empty") {
        val result = PushInterpreter(PushInterpreterState.empty).run()
        result should be(PushInterpreterState.empty)

        val result2 = PushInterpreter(PushInterpreterState.empty.pushInt(3)).run()
        result should be (PushInterpreterState.empty.pushInt(3))
      }


    }
  }
}
