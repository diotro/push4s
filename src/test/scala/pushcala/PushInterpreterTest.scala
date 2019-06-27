package pushcala

import org.scalatest.{FunSpec, Matchers}

class PushInterpreterTest extends FunSpec with Matchers {
  describe("PushInterpreter") {
    describe("run()") {
      it ("should return immediately if exec is empty") {
        val result = PushInterpreter(PushInterpreterState.empty).run()
        result should be(PushInterpreterState.empty)

        val result2 = PushInterpreter(PushInterpreterState.empty.pushInt(3)).run()
        result2 should be (PushInterpreterState.empty.pushInt(3))
      }
    }

    describe("step()") {
      it ("should do nothing if exec is empty") {
        val before = PushInterpreter(PushInterpreterState.empty)
        before.state should be (before.step().state)
      }

      it ("can handle booleans") {
        val before = PushInterpreter(
          PushInterpreterState.empty.pushExec(LiteralBoolean(true)))

        val after = before.step()
        after.state.popBoolean()._1 should contain (true)
        after.state.popExec()._1 should be (None)
      }

      it ("can handle strings") {
        val before = PushInterpreter(
          PushInterpreterState.empty.pushExec(LiteralString("ok")))

        val after = before.step()
        after.state.popString()._1 should contain ("ok")
        after.state.popExec()._1 should be (None)
      }

      it ("can handle ints") {
        val before = PushInterpreter(
          PushInterpreterState.empty.pushExec(LiteralInt(-100)))

        val after = before.step()
        after.state.popInt()._1 should contain (-100)
        after.state.popExec()._1 should be (None)
      }

      it ("can handle floats") {
        val before = PushInterpreter(
          PushInterpreterState.empty.pushExec(LiteralFloat(12.3f)))

        val after = before.step()
        after.state.popFloat()._1 should contain (12.3f)
        after.state.popExec()._1 should be (None)
      }

      it ("can handle instructions") {
        val before = PushInterpreter(
          PushInterpreterState.empty.pushExec(Instruction.fromName("boolean_not").get))

        val after = before.step()
        after.state shouldBe PushInterpreterState.empty
      }

      it ("can handle lists") {
        val start = PushInterpreter(
          PushInterpreterState.fromProgram(
            PushParser.parse("(true 3 4)").get))
        val out = start.step()
        out.state.popBoolean()._1 should contain(true)
        // the four is pushed after the three, so should be retrieved first
        out.state.popInt()._1 should contain(4)
      }

      it ("should run instructions") {
        val before = PushInterpreter(
          PushInterpreterState.empty
            .pushBoolean(true)
            .pushExec(Instruction.fromName("boolean_not").get))

        val after = before.step()
        after.state.popExec()._1 shouldBe None
        after.state.popBoolean()._1 should contain(false)
      }
    }
  }
}
