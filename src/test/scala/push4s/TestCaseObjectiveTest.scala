package push4s

import org.scalatest.{FunSpec, Matchers}

class TestCaseObjectiveTest extends FunSpec with Matchers {
  describe("Objective") {
    it ("can score ints by distance") {
      val obj = TestCaseObjective(TestCase(Seq(PushInt(1)), Seq(PushInt(1))))
      obj.score(PushParser.parse("3").get) shouldBe Seq(2)
    }
    // TODO: Test other types of objectives
  }

}
