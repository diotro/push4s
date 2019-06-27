package pushcala

sealed trait PushAtom

sealed trait Literal[T] extends PushAtom {
  def value: T
}

case class LiteralInt(value: Int) extends Literal[Int]
case class LiteralFloat(value: Float) extends Literal[Float]
case class LiteralString(value: String) extends Literal[String]
case class LiteralBoolean(value: Boolean) extends Literal[Boolean]


case class Instruction (name: String) extends PushAtom {
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

  // Hide apply constructor
  private def apply(): Unit = {}
}

case class PushList(contents: PushProgram) extends PushAtom

