package push4s

import org.scalatest.{FunSpec, Matchers}

class BenchmarkLoaderTest extends FunSpec with Matchers {
  describe("BenchmarkLoader") {
    it("can read benchmarks in files") {
      val result = BenchmarkLoader.loadFromFile("src/main/resources/collatz_benchmark.json")
      assert(result.get.name == "collatz")
      assert(result.get.trainingTestCases.head ==
        TestCase(Seq(PushInt(1)), Seq(PushInt(1))))
    }
  }

}
