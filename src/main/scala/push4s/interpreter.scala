package push4s

/** A push interpreter, with a specific state.
  *
  * @param state The state of this push interpreter.
  */
case class PushInterpreter(state: PushInterpreterState) {

  /** @return This interpreter, after one additional step. */
  def step(): PushInterpreter = {
    val (nextInstruction, newState) = this.state.popExec()
    val nextState = nextInstruction match {
      case Some(atom: PushElement) => processAtom(newState, atom)
      case None                    => newState
    }
    PushInterpreter(nextState)
  }

  private def processAtom(state: PushInterpreterState,
                          atom: PushElement): PushInterpreterState = {
    atom match {
      case PushList(atoms) =>
        atoms.foldRight(state) {
          case (nextAtom, accumulatedState) =>
            accumulatedState.pushExec(nextAtom)
        }
      case PushBoolean(b)     => state.pushBoolean(b)
      case PushInt(i)         => state.pushInt(i)
      case PushFloat(f)       => state.pushFloat(f)
      case PushString(s)      => state.pushString(s)
      case i: PushInstruction => Instructions.getDef(i)(state)
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

  /** Pushes the given inputs and program to the exec stack (in that order, so inputs are on top)
    * and then evaluates.
    */
  def runProgramWithInputs(program: PushProgram,
                           inputs: Seq[PushElement]): PushInterpreterState = {
    PushInterpreter.runProgram(inputs ++ program)
  }
}

case class PushInterpreterState(exec: PushStack[PushElement],
                                code: PushStack[PushElement],
                                int: PushStack[Int],
                                float: PushStack[Float],
                                string: PushStack[String],
                                boolean: PushStack[Boolean]) {

  /** Pushes an arbitrary literal onto the interpreter state. */
  def pushElement(value: PushElement): PushInterpreterState = {
    value match {
      case PushInt(i)         => this.pushInt(i)
      case PushFloat(f)       => this.pushFloat(f)
      case PushString(s)      => this.pushString(s)
      case PushBoolean(b)     => this.pushBoolean(b)
      case i: PushInstruction => this.pushExec(i)
      case l: PushList        => this.pushExec(l)
    }
  }
  /** Pushes an arbitrary literal onto the interpreter state. */
  def pushElement(value: Option[PushElement]): PushInterpreterState = {
    value.fold(this) {
      case PushInt(i)         => this.pushInt(i)
      case PushFloat(f)       => this.pushFloat(f)
      case PushString(s)      => this.pushString(s)
      case PushBoolean(b)     => this.pushBoolean(b)
      case i: PushInstruction => this.pushExec(i)
      case l: PushList        => this.pushExec(l)
    }
  }

  // There is a lot of code repetition here, as each stack needs an implementation
  // of each helper method. Downsides of a type system.
  def pushExec(x: PushElement): PushInterpreterState =
    this.copy(exec = exec.push(x))

  def popExec(): (Option[PushElement], PushInterpreterState) = {
    val (result, newStack) = exec.pop()
    (result, this.copy(exec = newStack))
  }

  def pop2Exec(): (Option[(PushElement, PushElement)], PushInterpreterState) = {
    val (result, newStack) = exec.pop2()
    (result, this.copy(exec = newStack))
  }

  def mapExec(func: PushElement => PushElement): PushInterpreterState = {
    val (boolean, newState) = this.popExec()
    boolean.fold(this)(i => newState.pushExec(func(i)))
  }

  def mapExec(
    func: (PushElement, PushElement) => PushElement
  ): PushInterpreterState = {
    val (booleans, newState) = this.pop2Exec()
    booleans.fold(this) { case (i1, i2) => newState.pushExec(func(i1, i2)) }
  }

  def mapExecToAny(func: PushElement => PushElement): PushInterpreterState = {
    val (boolean, newState) = this.popExec()
    boolean.fold(this)(i => newState.pushExec(func(i)))
  }

  def mapExecToAny(
    func: (PushElement, PushElement) => PushElement
  ): PushInterpreterState = {
    val (booleans, newState) = this.pop2Exec()
    booleans.fold(this) { case (i1, i2) => newState.pushElement(func(i1, i2)) }
  }

  def pushCode(x: PushElement): PushInterpreterState =
    this.copy(code = code.push(x))

  def popCode(): (Option[PushElement], PushInterpreterState) = {
    val (result, newStack) = code.pop()
    (result, this.copy(code = newStack))
  }

  def pop2Code(): (Option[(PushElement, PushElement)], PushInterpreterState) = {
    val (result, newStack) = code.pop2()
    (result, this.copy(exec = newStack))
  }

  def mapCode(func: PushElement => PushElement): PushInterpreterState = {
    val (boolean, newState) = this.popCode()
    boolean.fold(this)(i => newState.pushExec(func(i)))
  }

  def mapCode(
    func: (PushElement, PushElement) => PushElement
  ): PushInterpreterState = {
    val (booleans, newState) = this.pop2Code()
    booleans.fold(this) { case (i1, i2) => newState.pushCode(func(i1, i2)) }
  }

  def mapCodeList(func: PushList => PushElement): PushInterpreterState = {
    val (code, newState) = this.popCode()
    code.fold(this)({
      case l: PushList => newState.pushElement(func(l))
      case _           => this
    })
  }

  def mapCodeNonEmptyList(
    func: PushList => PushElement
  ): PushInterpreterState = {
    val (code, newState) = this.popCode()
    code.fold(this)({
      case l: PushList =>
        if (l.isEmpty) {
          this
        } else {
          newState.pushElement(func(l))
        }
      case _ => this
    })
  }

  def mapCodeToAny(func: PushElement => PushElement): PushInterpreterState = {
    val (boolean, newState) = this.popCode()
    boolean.fold(this)(i => newState.pushCode(func(i)))
  }

  def mapCodeToAny(
    func: (PushElement, PushElement) => PushElement
  ): PushInterpreterState = {
    val (booleans, newState) = this.pop2Code()
    booleans.fold(this) { case (i1, i2) => newState.pushElement(func(i1, i2)) }
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

  def mapIntToAny(func: Int => PushElement): PushInterpreterState = {
    val (int, newState) = this.popInt()
    int.fold(this)(i => newState.pushElement(func(i)))
  }

  def mapIntToAny(func: (Int, Int) => PushElement): PushInterpreterState = {
    val (ints, newState) = this.pop2Ints()
    ints.fold(this) { case (i1, i2) => newState.pushElement(func(i1, i2)) }
  }

  def pushFloat(f: Float): PushInterpreterState =
    this.copy(float = float.push(f))

  def popFloat(): (Option[Float], PushInterpreterState) = {
    val (result, newStack) = float.pop()
    (result, this.copy(float = newStack))
  }

  def pop2Floats(): (Option[(Float, Float)], PushInterpreterState) = {
    val (result, newStack) = float.pop2()
    (result, this.copy(float = newStack))
  }

  def mapFloat(func: Float => Float): PushInterpreterState = {
    val (float, newState) = this.popFloat()
    float.fold(this)(i => newState.pushFloat(func(i)))
  }

  def mapFloat(func: (Float, Float) => Float): PushInterpreterState = {
    val (floats, newState) = this.pop2Floats()
    floats.fold(this) { case (i1, i2) => newState.pushFloat(func(i1, i2)) }
  }

  def mapFloatToAny(func: Float => PushElement): PushInterpreterState = {
    val (float, newState) = this.popFloat()
    float.fold(this)(i => newState.pushElement(func(i)))
  }

  def mapFloatToAny(
    func: (Float, Float) => PushElement
  ): PushInterpreterState = {
    val (floats, newState) = this.pop2Floats()
    floats.fold(this) { case (i1, i2) => newState.pushElement(func(i1, i2)) }
  }

  def pushString(s: String): PushInterpreterState =
    this.copy(string = string.push(s))

  def popString(): (Option[String], PushInterpreterState) = {
    val (result, newStack) = string.pop()
    (result, this.copy(string = newStack))
  }

  def pop2Strings(): (Option[(String, String)], PushInterpreterState) = {
    val (result, newStack) = string.pop2()
    (result, this.copy(string = newStack))
  }

  def mapString(func: String => String): PushInterpreterState = {
    val (string, newState) = this.popString()
    string.fold(this)(i => newState.pushString(func(i)))
  }

  def mapString(func: (String, String) => String): PushInterpreterState = {
    val (strings, newState) = this.pop2Strings()
    strings.fold(this) { case (i1, i2) => newState.pushString(func(i1, i2)) }
  }

  def mapStringToAny(func: String => PushElement): PushInterpreterState = {
    val (string, newState) = this.popString()
    string.fold(this)(i => newState.pushElement(func(i)))
  }

  def mapStringToAny(
    func: (String, String) => PushElement
  ): PushInterpreterState = {
    val (strings, newState) = this.pop2Strings()
    strings.fold(this) { case (i1, i2) => newState.pushElement(func(i1, i2)) }
  }

  def pushBoolean(b: Boolean): PushInterpreterState =
    this.copy(boolean = boolean.push(b))

  def popBoolean(): (Option[Boolean], PushInterpreterState) = {
    val (result, newStack) = boolean.pop()
    (result, this.copy(boolean = newStack))
  }

  def pop2Booleans(): (Option[(Boolean, Boolean)], PushInterpreterState) = {
    val (result, newStack) = boolean.pop2()
    (result, this.copy(boolean = newStack))
  }

  def mapBoolean(func: Boolean => Boolean): PushInterpreterState = {
    val (boolean, newState) = this.popBoolean()
    boolean.fold(this)(i => newState.pushBoolean(func(i)))
  }

  def mapBoolean(func: (Boolean, Boolean) => Boolean): PushInterpreterState = {
    val (booleans, newState) = this.pop2Booleans()
    booleans.fold(this) { case (i1, i2) => newState.pushBoolean(func(i1, i2)) }
  }

  def mapBooleanToAny(func: Boolean => PushAtom[_]): PushInterpreterState = {
    val (boolean, newState) = this.popBoolean()
    boolean.fold(this)(i => newState.pushElement(func(i)))
  }

  def mapBooleanToAny(
    func: (Boolean, Boolean) => PushAtom[_]
  ): PushInterpreterState = {
    val (booleans, newState) = this.pop2Booleans()
    booleans.fold(this) { case (i1, i2) => newState.pushElement(func(i1, i2)) }
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
      case Nil       => (None, this)
    }
  }

  def pop2(): (Option[(T, T)], PushStack[T]) = {
    this.pop() match {
      case (Some(i1), newStack) =>
        newStack.pop() match {
          case (Some(i2), newStack2) => (Some(i1, i2), newStack2)
          case _                     => (None, this)
        }
      case _ => (None, this)
    }
  }
}
