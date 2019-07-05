package pushcala

/** A push interpreter, with a specific state.
  *
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
      case PushList(atoms) => atoms.foldRight(state) {
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
    PushInterpreter.runProgram(inputs ++ program)
  }
}


case class PushInterpreterState(exec: PushStack[PushAtom],
                                code: PushStack[PushAtom],
                                int: PushStack[Int],
                                float: PushStack[Float],
                                string: PushStack[String],
                                boolean: PushStack[Boolean]) {
  /** Pushes an arbitrary literal onto the interpreter state. */
  def pushLiteral(value: PushLiteral[_]): PushInterpreterState = {
    value match {
      case LiteralInt(i) => this.pushInt(i)
      case LiteralFloat(f) => this.pushFloat(f)
      case LiteralString(s) => this.pushString(s)
      case LiteralBoolean(b) => this.pushBoolean(b)
    }
  }

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

  def pushInt(i: Int): PushInterpreterState = this.copy(int = int.push(i))

  def popInt(): (Option[Int], PushInterpreterState) = {
    val (result, newStack) = int.pop()
    (result, this.copy(int = newStack))
  }

  def pop2Ints(): (Option[(Int, Int)], PushInterpreterState) = {
    val (result, newStack) = int.pop2()
    (result, this.copy(int = newStack))
  }

  def mapInt(func: Int => Int): PushInterpreterState = {
    val (int, newState) = this.popInt()
    int.fold(this)(i => newState.pushInt(func(i)))
  }

  def mapInt(func: (Int, Int) => Int): PushInterpreterState = {
    val (ints, newState) = this.pop2Ints()
    ints.fold(this) { case (i1, i2) => newState.pushInt(func(i1, i2)) }
  }

  def mapIntToAny(func: Int => PushLiteral[_]): PushInterpreterState = {
    val (int, newState) = this.popInt()
    int.fold(this)(i => newState.pushLiteral(func(i)))
  }

  def mapIntToAny(func: (Int, Int) => PushLiteral[_]): PushInterpreterState = {
    val (ints, newState) = this.pop2Ints()
    ints.fold(this) { case (i1, i2) => newState.pushLiteral(func(i1, i2)) }
  }

  def pushFloat(f: Float): PushInterpreterState = this.copy(float = float.push(f))

  def popFloat(): (Option[Float], PushInterpreterState) = {
    val (result, newStack) = float.pop()
    (result, this.copy(float = newStack))
  }

  def pop2Floats(): (Option[(Float, Float)], PushInterpreterState) = {
    val (result, newStack) = float.pop2()
    (result, this.copy(float = newStack))
  }

  def pushString(s: String): PushInterpreterState = this.copy(string = string.push(s))

  def popString(): (Option[String], PushInterpreterState) = {
    val (result, newStack) = string.pop()
    (result, this.copy(string = newStack))
  }

  def pop2Strings(): (Option[(String, String)], PushInterpreterState) = {
    val (result, newStack) = string.pop2()
    (result, this.copy(string = newStack))
  }

  def pushBoolean(b: Boolean): PushInterpreterState = this.copy(boolean = boolean.push(b))

  def popBoolean(): (Option[Boolean], PushInterpreterState) = {
    val (result, newStack) = boolean.pop()
    (result, this.copy(boolean = newStack))
  }

  def pop2Booleans(): (Option[(Boolean, Boolean)], PushInterpreterState) = {
    val (result, newStack) = boolean.pop2()
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

  def pop2(): (Option[(T, T)], PushStack[T]) = {
    this.pop() match {
      case (Some(i1), newStack) =>
        newStack.pop() match {
          case (Some(i2), newStack2) => (Some(i1, i2), newStack2)
          case _ => (None, this)
        }
      case _ => (None, this)
    }
  }
}
