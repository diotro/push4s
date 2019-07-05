package pushcala

object RunBenchmark {
  def main(args: Array[String]): Unit = {
    val filename = args(0)
    val benchmark = BenchmarkLoader.loadFromFile(filename).get

    // Read the program as the second argument - No GP yet.
    println(args(1))
    val program = PushParser.parse(args(1)).get

    println(benchmark.evaluate(program))
  }
}
