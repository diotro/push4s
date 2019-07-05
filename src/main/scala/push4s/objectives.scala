package push4s


/** The objective function for a Push benchmark test case. */
case class Objective(testCase: TestCase) {

  def score(program: PushProgram): Seq[Double] = {
    val result = PushInterpreter.runProgramWithInputs(program, testCase.inputs)

    // Starting from the result and an empty list of scores, gradually add scores based on output
    // and remove the atoms that have been popped and scored.
    testCase.outputs.foldLeft(
      (result, Seq[Double]())
    )(scoreOneOutput)._2
  }

  private def scoreOneOutput(stateAndScore: (PushInterpreterState, Seq[Double]),
                             expectedOutput: PushLiteral[_])
  : (PushInterpreterState, Seq[Double]) = {
    val (state: PushInterpreterState, scores: Seq[Double]) = stateAndScore

    expectedOutput match {
      case LiteralBoolean(b) =>
        state.popBoolean() match {
          case (maybeBool, newState) =>
            // The score is 1 if the top boolean is equal to the boolean specified
            (newState, scores :+ maybeBool.map(a => if (a == b) 1d else 0d).getOrElse(0d))
        }
      case LiteralInt(i) =>
        state.popInt() match {
          case (maybeInt, newState) =>
            // Score based on distance. If there is no int in the stack, assume 0f.
            (newState, scores :+ math.abs(maybeInt.getOrElse(0) - i))
        }
      case LiteralFloat(i) =>
        state.popFloat() match {
          case (maybeFloat, newState) =>
            // Score based on distance. If there is no float in the stack, assume 0f.
            (newState, scores :+ math.abs(maybeFloat.getOrElse(0f) - i))
        }
      case LiteralString(s) =>
        state.popString() match {
          case (maybeString, newState) =>
            (newState, scores :+ Levenshtein.distance(maybeString.getOrElse(""), s))
        }
    }
  }
}

object Levenshtein {
  // Ripped straight from
  // https://github.com/vickumar1981/stringdistance/blob/master/src/main/scala/com/github/vickumar1981/stringdistance/impl/LevenshteinDistanceImpl.scala
  // which is not updated for 2.13.
  def distance(a: String, b: String): Int =
    ((0 to b.length).toList /: a) ((prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) => math.min(math.min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }).last
}
