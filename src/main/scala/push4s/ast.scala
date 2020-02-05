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
case class PushInstruction(name: String) extends PushElement

object PushInstruction {

  /** @return The instruction with the given name, if there is one. */
  def fromName(name: String): Option[PushInstruction] = {
    if (PushInstruction.exists(name)) {
      Some(PushInstruction(name))
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
case class PushList(contents: PushProgram) extends PushElement {
  def isEmpty: Boolean = contents.isEmpty
  def length: Int = contents.length
}
object PushList {

  /**
    * Produces a list from the two elements. If they are lists, combines them. If either or both
    * are atoms, puts them in lists.
    */
  def from(e1: PushElement, e2: PushElement): PushList = {
    val l1 = e1 match {
      case l: PushList => l.contents
      case _ => Seq(e1)
    }
    val l2 = e2 match {
      case l: PushList => l.contents
      case _ => Seq(e1)
    }

   PushList(l1 ++ l2)
  }
}
