package pushcala


case class Interpreter(state: PushInterpreterState) {
  /**
    * @return This interpreter, after one additional step
    */
  def step(): Interpreter = ???

  /** @return The state after running from the given state until the exec stack is empty. */
  def run(): PushInterpreterState = ???
}



case class PushInterpreterState(exec: PushStack[PushAtom],
                                code: PushStack[PushAtom],
                                int: PushStack[Int],
                                float: PushStack[Float],
                                string: PushStack[String],
                                boolean: PushStack[Boolean]) {
  def topInt(): Option[Int] = int.contents.headOption
  def topInts(n: Int): List[Int] = int.contents.take(n)
  def topFloat(): Option[Float] = float.contents.headOption
  def topFloats(n: Int): List[Float] = float.contents.take(n)
  def topExec(): Option[PushAtom] = exec.contents.headOption
  def topExecs(n: Int): List[PushAtom] = exec.contents.take(n)
  def topCode(): Option[PushAtom] = code.contents.headOption
  def topCodes(n: Int): List[PushAtom] = code.contents.take(n)
  def topString(): Option[String] = string.contents.headOption
  def topStrings(n: Int): List[String] = string.contents.take(n)
  def topBoolean(): Option[Boolean] = boolean.contents.headOption
  def topBooleans(n: Int): List[Boolean] = boolean.contents.take(n)


}

object PushInterpreterState {
  /**
    * @return The Push interpreter state corresponding to the given program
    */
  def startProgram(program: PushProgram): PushInterpreterState = {
    // All stacks are empty except for the exec stack, which has the contents of the program.
    PushInterpreterState(
      PushStack(program.toList),
      PushStack(List()),
      PushStack(List()),
      PushStack(List()),
      PushStack(List()),
      PushStack(List()),
    )
  }
}

/**
  * A single stack in the push interpreter state.
  * @param contents The contents of the stack.
  * @tparam T The type of the contents of the stack.
  */
case class PushStack[T](contents: List[T])
