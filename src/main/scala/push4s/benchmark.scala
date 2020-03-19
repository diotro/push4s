package push4s

import io.evvo.island.population.{Minimize, Objective}
import org.json4s._
import org.json4s.native.JsonMethods

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

  /** @return The objectives that should be used for training programs for this benchmark. */
  def objectives: Seq[TestCaseObjective] =
    this.trainingTestCases.map(TestCaseObjective)

  /** @return The results of evaluating the given program against each of the evaluation test
    *         cases.
    */
  def evaluate(program: PushProgram): Seq[Double] = {
    this.evaluationTestCases.map(TestCaseObjective).flatMap(_.score(program))
  }

  def toObjectives: Seq[Objective[PushProgram]] = {
    val bunchSize: Int = 20
    trainingTestCases.grouped(bunchSize).zipWithIndex.map {
      case (testCases, index) =>
        val start = index * bunchSize
        new Objective[PushProgram](f"$name:$start-${start+bunchSize}", Minimize()) {
          private val scorers = testCases.map(TestCaseObjective)
          override protected def objective(sol: PushProgram): Double = {
            scorers.map(_.score(sol).sum).sum
          }
        }
    }.toVector
  }

  /** Does that program pass this benchmark? That is, does it score "0" on each objective? */
  def passedBy(program: PushProgram): Boolean = {
    this.evaluate(program).sum == 0d
  }
}

/** A single test case, where the given inputs should lead to the specified output. */
case class TestCase(inputs: Seq[PushElement], outputs: Seq[PushAtom[_]])

case class ParsedBenchmark(name: String,
                           trainingTestCases: List[ParsedTestCase],
                           evaluationTestCases: List[ParsedTestCase],
                           includedStacks: List[String]) {
  def stringToPushStack(string: String): PushStackType = {
    string match {
      case "Boolean" => BooleanStack
      case "Int"     => IntStack
      case "Float"   => FloatStack
      case "String"  => StringStack
    }
  }

  def toBenchmark: Benchmark = {
    Benchmark(
      name,
      trainingTestCases.map(_.toTestCase).toVector,
      evaluationTestCases.map(_.toTestCase).toVector,
      includedStacks.map(stringToPushStack).toVector,
    )
  }
}

case class ParsedTestCase(in: List[JValue], out: List[JValue]) {
  def jvalueToPushLiteral(j: JValue): PushAtom[_] = {
    j match {
      case JBool(b)   => PushBoolean(b)
      case JInt(i)    => PushInt(i.toInt)
      case JDouble(f) => PushFloat(f.toFloat)
      case JString(s) => PushString(s)
      case _          => throw new IllegalArgumentException("Must be literal type.")
    }
  }

  def toTestCase: TestCase = {
    TestCase(in.map(jvalueToPushLiteral), out.map(jvalueToPushLiteral))
  }
}

object BenchmarkLoader {
  implicit val formats: Formats = DefaultFormats

  /** Loads a benchmark from the given Source. */
  def load(source: Source): Option[Benchmark] = {
    JsonMethods
      .parseOpt(source.mkString)
      .map(_.extract[ParsedBenchmark])
      .map(_.toBenchmark)
  }

  /** Loads a benchmark from the given file. */
  def loadFromFile(fileName: String): Option[Benchmark] =
    this.load(Source.fromFile(fileName))
}
