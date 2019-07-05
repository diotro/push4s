package push4s

/** The units of a push program. Everything that can be parsed (lists included) extends
  * this class.
  */
sealed trait PushAtom

/** A literal with a value of type T. */
sealed trait PushLiteral[T] extends PushAtom {
  def value: T
}

case class LiteralInt(value: Int) extends PushLiteral[Int]
case class LiteralFloat(value: Float) extends PushLiteral[Float]
case class LiteralString(value: String) extends PushLiteral[String]
case class LiteralBoolean(value: Boolean) extends PushLiteral[Boolean]


/** Represents an instruction, referenced by name. Use Instructions.getDef(…) to retrieve
  * the actual function underlying this instruction.
  * @param name The name of this instruction.
  */
case class Instruction (name: String) extends PushAtom

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
case class PushList(contents: PushProgram) extends PushAtom

