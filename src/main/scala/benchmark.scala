import pushcala.{BooleanStack, FloatStack, IntStack, LiteralBoolean, LiteralFloat, LiteralInt, LiteralString, PushAtom, PushStackType, StringStack}

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._


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
                     includedStacks: Seq[PushStackType])


/** A single test case, where the given inputs should lead to the specified output. */
case class TestCase(inputs: Seq[PushAtom], outputs: Seq[PushAtom])


case class ParsedBenchmark(name: String,
                           trainingTestCases: Seq[ParsedTestCase],
                           evaluationTestCases: Seq[ParsedTestCase],
                           includedStacks: Seq[String]) {
  println(this)

  def stringToPushStack(string: String): PushStackType = {
    string match {
      case "boolean" => BooleanStack
      case "int" => IntStack
      case "float" => FloatStack
      case "string" => StringStack
    }
  }

  def toBenchmark: Benchmark = {
    Benchmark(
      name,
      trainingTestCases.map(_.toTestCase),
      evaluationTestCases.map(_.toTestCase),
      includedStacks.map(stringToPushStack))
  }
}

case class ParsedTestCase(in: Seq[JValue], out: Seq[JValue]) {
  def jvalueToPushAtom(j: JValue): PushAtom = {
    j match {
      case JBool(b) => LiteralBoolean(b)
      case JInt(i) => LiteralInt(i.toInt)
      case JDouble(f) => LiteralFloat(f.toFloat)
      case JString(s) => LiteralString(s)
      case _ => throw new IllegalArgumentException()
    }
  }

  def toTestCase: TestCase = {
    TestCase(in.map(jvalueToPushAtom), out.map(jvalueToPushAtom))
  }
}

object BenchmarkLoader {
  implicit val formats: Formats = DefaultFormats

  /** Loads a benchmark from the given Source. */
  def load(source: Source): Option[Benchmark] =
    parseOpt(source.mkString)
      .map(_.extract[ParsedBenchmark])
      .map(_.toBenchmark)

  /** Loads a benchmark from the given file. */
  def loadFromFile(fileName: String): Option[Benchmark] = this.load(io.Source.fromFile(fileName))
}
