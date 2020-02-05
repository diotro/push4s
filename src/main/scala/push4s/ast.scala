package push4s

/** The units of a push program. Everything that can be parsed (lists included) extends
  * this class.
  */
sealed trait PushElement

/** A literal with a value of type T. */
sealed trait PushAtom[T] extends PushElement {
  def value: T
}

case class PushInt(value: Int) extends PushAtom[Int]
case class PushFloat(value: Float) extends PushAtom[Float]
case class PushString(value: String) extends PushAtom[String]
case class PushBoolean(value: Boolean) extends PushAtom[Boolean]


/** Represents an instruction, referenced by name. Use Instructions.getDef(…) to retrieve
  * the actual function underlying this instruction.
  * @param name The name of this instruction.
  */
case class Instruction (name: String) extends PushElement

object Instruction {
  /** @return The instruction with the given name, if there is one. */
  def fromName(name: String): Option[Instruction] = {
    if (Instruction.exists(name)) {
      Some(Instruction(name))
    } else {
      None
    }
  }

  /** @return Whether there is an instruction with the given name. */
  def exists(name: String): Boolean = Instructions.containsDefinitionFor(name)

  // Hide apply constructor, so you can't make an instruction without using `fromName`.
  private def apply(): Unit = {}
}

/** A list contains is just a push program itself. */
case class PushList(contents: PushProgram) extends PushElement

