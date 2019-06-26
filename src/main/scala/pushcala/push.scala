package pushcala

sealed trait PushAtom

sealed trait Literal[T] extends PushAtom {
  def value: T
}

case class LiteralInt(value: Int) extends Literal[Int]
case class LiteralFloat(value: Float) extends Literal[Float]
case class LiteralString(value: String) extends Literal[String]
case class LiteralBoolean(value: Boolean) extends Literal[Boolean]


class Instruction (private val name: String) extends PushAtom {
  override def hashCode(): Int = {
    this.name.hashCode()
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Instruction => this.name == that.name
    case _ => false
  }
}


object Instruction {
  def fromName(name: String): Option[Instruction] = {
    if (Instruction.exists(name)) {
      Some(new Instruction(name))
    } else {
      None
    }
  }

  def exists(name: String): Boolean = {
    Instructions.containsDefinitionFor(name)
  }
}

case class PushList(contents: PushProgram) extends PushAtom

case class PushInterpreterState(exec: PushStack[PushAtom],
                                code: PushStack[PushAtom],
                                int: PushStack[Int],
                                float: PushStack[Float],
                                string: PushStack[String],
                                boolean: PushStack[Boolean])

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
