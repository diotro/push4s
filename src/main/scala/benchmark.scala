import pushcala.{PushAtom, PushStack, PushStackType}

import scala.io.Source

/** A benchmark for program synthesis.
  *
  * @param name                The name of the benchmark.
  * @param trainingTestCases   The set of input-output pairs to train the program with.
  * @param evaluationTestCases The set of input-output pairs to evaluate the program with.
  * @param includedStacks      Which stacks (and therefore, which instructions) to include.
  */
case class Benchmark(name: String,
                     trainingTestCases: Seq[TestCase],
                     evaluationTestCases: Seq[TestCase],
                     includedStacks: Seq[PushStackType]) {

}

/** A single test case, where the given inputs should lead to the specified output. */
case class TestCase(inputs: Seq[PushAtom], outputs: Seq[PushAtom])


object BenchmarkLoader {
//  def load(source: Source): Benchmark = {
//
//  }
}
