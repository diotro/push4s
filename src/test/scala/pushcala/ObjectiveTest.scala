package pushcala

import org.scalatest.{FunSpec, Matchers}

class ObjectiveTest extends FunSpec with Matchers {
  describe("Objective") {
    it ("can score ints by distance") {
      val obj = Objective(TestCase(Seq(LiteralInt(1)), Seq(LiteralInt(1))))
      obj.score(PushInterpreter.parseAndRun("3").get) shouldBe Seq(2)
    }
  }

}
