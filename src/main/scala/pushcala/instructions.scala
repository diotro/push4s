package pushcala


// This object holds the map of all instruction names to the actual instruction.
object Instructions {
  def getDef(i: Instruction): InstructionDef = map(i.name)

  private val map: Map[String, InstructionDef] = List(
    BooleanNot
  ).map(x => x.name -> x).toMap

  def apply(name: String): Option[InstructionDef] = map.get(name)

  def containsDefinitionFor(name: String): Boolean = map.contains(name)
}


// Instructions are functions that accept a push state and produce a new one
// Although they can be referenced by symbols
sealed trait InstructionDef extends (PushInterpreterState => PushInterpreterState) {
  val name: String
  val stack: PushStackType
  override def apply(state: PushInterpreterState): PushInterpreterState
}

abstract class AbstractInstruction(val name: String, val stack: PushStackType)
  extends InstructionDef

object BooleanNot extends AbstractInstruction("boolean_not", BooleanStack) {

  override def apply(state: PushInterpreterState): PushInterpreterState = {
    val newBooleanStack = state.boolean.contents match {
      case firstBool :: rest => !firstBool :: rest
      case _ => List()
    }

    state.copy(boolean = PushStack[Boolean](newBooleanStack))
  }
}
