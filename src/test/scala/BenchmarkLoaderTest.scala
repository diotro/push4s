import org.scalatest.{FunSpec, Matchers}
import pushcala.LiteralInt

class BenchmarkLoaderTest extends FunSpec with Matchers {
  describe("Benchmark Loader") {
    it("can read benchmarks in files") {
      val result = BenchmarkLoader.loadFromFile("src/main/resources/benchmark1.json")
      assert(result.get.name == "benchmark1")
      assert(result.get.trainingTestCases.head ==
        TestCase(Seq(LiteralInt(3), LiteralInt(4)), Seq(LiteralInt(7))))
    }
  }

}
