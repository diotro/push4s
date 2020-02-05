package push4s

import org.scalatest.{FunSpec, Matchers}

class TestBenchmark extends FunSpec with Matchers {
  describe("A benchmark") {
    it("should score programs on their number correct") {
      val b = Benchmark(
        "benchmark",
        Seq(),
        Seq(TestCase(Seq(), Seq(PushInt(3))), TestCase(Seq(), Seq(PushString("hello")))),
        Seq(IntStack)
      )
      b.evaluate(Seq(PushInt(3))) shouldBe Seq(0, 5)
      b.evaluate(Seq(PushString("hell"))) shouldBe Seq(3, 1)
    }
  }
}
