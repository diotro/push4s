package pushcala

/** A push interpreter, with a specific state.
  * @param state The state of this push interpreter.
  */
case class PushInterpreter(state: PushInterpreterState) {
  /** @return This interpreter, after one additional step. */
  def step(): PushInterpreter = {
    val (nextInstruction, newState) = this.state.popExec()
    val nextState = nextInstruction match {
      case Some(atom: PushAtom) => processAtom(newState, atom)
      case None => newState
    }
    PushInterpreter(nextState)
  }

  private def processAtom(state: PushInterpreterState, atom: PushAtom): PushInterpreterState = {
    atom match {
      case PushList(atoms) => atoms.foldRight(state){
        case (nextAtom, accumulatedState) => accumulatedState.pushExec(nextAtom)
      }
      case LiteralBoolean(b) => state.pushBoolean(b)
      case LiteralInt(i) => state.pushInt(i)
      case LiteralFloat(f) => state.pushFloat(f)
      case LiteralString(s) => state.pushString(s)
      case i: Instruction => Instructions.getDef(i)(state)
    }
  }

  /** @return The state after running from the given state until the exec stack is empty. */
  def run(): PushInterpreterState = {
    if (this.state.exec.contents.isEmpty) {
      this.state
    } else {
      this.step().run()
    }
  }
}

object PushInterpreter {
  /** Creates a Push Interpreter, with starting state equal to just
    * the given program.
    *
    * @param program The program to run.
    * @return
    */
  def fromProgram(program: PushProgram): PushInterpreter = {
    PushInterpreter(PushInterpreterState.fromProgram(program))
  }

  /** Runs the specified program.
    *
    * @param program The instructions to execute.
    * @return The state of the interpreter once the exec stack is empty.
    */
  def runProgram(program: PushProgram): PushInterpreterState = {
    PushInterpreter.fromProgram(program).run()
  }

  /** Parses and runs the given program, represented as a string.
    *
    * @param program The program to run.
    * @return either the state at the end of running, or None if the
    *         program, was unparseable.
    */
  def parseAndRun(program: String): Option[PushInterpreterState] = {
    PushParser.parse(program).map(PushInterpreter.runProgram)
  }

  /** Pushes the given program and inputs to the exec stack (in that order - so program first)
    * and then runs the program.
    */
  def runProgramWithInputs(program: PushProgram, inputs: Seq[PushAtom]): PushInterpreterState = {
    PushInterpreter.runProgram(program ++ inputs)
  }
}


case class PushInterpreterState(exec: PushStack[PushAtom],
                                code: PushStack[PushAtom],
                                int: PushStack[Int],
                                float: PushStack[Float],
                                string: PushStack[String],
                                boolean: PushStack[Boolean]) {

  def pushExec(x: PushAtom): PushInterpreterState = this.copy(exec = exec.push(x))

  def popExec(): (Option[PushAtom], PushInterpreterState) = {
    val (result, newStack) = exec.pop()
    (result, this.copy(exec = newStack))
  }


  def pushCode(x: PushAtom): PushInterpreterState = this.copy(code = code.push(x))

  def popCode(): (Option[PushAtom], PushInterpreterState) = {
    val (result, newStack) = code.pop()
    (result, this.copy(code = newStack))
  }

  def pushInt(x: Int): PushInterpreterState = this.copy(int = int.push(x))

  def popInt(): (Option[Int], PushInterpreterState) = {
    val (result, newStack) = int.pop()
    (result, this.copy(int = newStack))
  }

  def pushFloat(x: Float): PushInterpreterState = this.copy(float = float.push(x))

  def popFloat(): (Option[Float], PushInterpreterState) = {
    val (result, newStack) = float.pop()
    (result, this.copy(float = newStack))
  }

  def pushString(x: String): PushInterpreterState = this.copy(string = string.push(x))

  def popString(): (Option[String], PushInterpreterState) = {
    val (result, newStack) = string.pop()
    (result, this.copy(string = newStack))
  }

  def pushBoolean(x: Boolean): PushInterpreterState = this.copy(boolean = boolean.push(x))

  def popBoolean(): (Option[Boolean], PushInterpreterState) = {
    val (result, newStack) = boolean.pop()
    (result, this.copy(boolean = newStack))
  }
}

object PushInterpreterState {
  /**
    * @return The Push interpreter state corresponding to the given program
    */
  def fromProgram(program: PushProgram): PushInterpreterState = {
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

  /**
    * @return A Push interpreter state with no data in it, useful for constructing examples and tests.
    */
  def empty: PushInterpreterState = PushInterpreterState(
    PushStack(List()),
    PushStack(List()),
    PushStack(List()),
    PushStack(List()),
    PushStack(List()),
    PushStack(List()),
  )
}

/** An enumeration of the types of Push stacks. */
sealed trait PushStackType
case object BooleanStack extends PushStackType
case object IntStack extends PushStackType
case object FloatStack extends PushStackType
case object StringStack extends PushStackType
case object ExecStack extends PushStackType
case object CodeStack extends PushStackType

/**
  * A single stack in the push interpreter state.
  *
  * @param contents The contents of the stack.
  * @tparam T The type of the contents of the stack.
  */
case class PushStack[T](contents: List[T]) {
  def push(t: T): PushStack[T] = {
    this.copy(t :: this.contents)
  }

  def pop(): (Option[T], PushStack[T]) = {
    this.contents match {
      case t :: more => (Some(t), PushStack(more))
      case Nil => (None, this)
    }
  }
}
