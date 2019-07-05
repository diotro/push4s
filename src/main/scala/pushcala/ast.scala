package pushcala

sealed trait PushAtom

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

  // Hide apply constructor
  private def apply(): Unit = {}
}

case class PushList(contents: PushProgram) extends PushAtom

