package pushcala

/** This object holds the map of all instruction names to the actual instruction.
  * It is a var, so that each extender of AbstractInstruction can modify the map to add itself.
  * It has to be private, so that no one else can modify it - they can use Instructions to access
  * it's fields.
  */
private object InstructionsMap {
  var map: Map[String, InstructionDef] = List(
    BooleanNot,
    IntegerAdd, IntegerSub
  ).map(i => i.name -> i).toMap
}

/** Used to get Instructions
  */
object Instructions {
  def getDef(i: Instruction): InstructionDef = InstructionsMap.map(i.name)
  def containsDefinitionFor(name: String): Boolean = InstructionsMap.map.contains(name)
}


/**Defines an instruction. Instructions must have a name, which is used to reference them
  * from Push code, as well as the stacks they are associated with
  */
sealed trait InstructionDef extends (PushInterpreterState => PushInterpreterState) {
  val name: String
  val stacks: Iterable[PushStackType]
  override def apply(state: PushInterpreterState): PushInterpreterState
}

/** Abstract class that all instructions should extend.
  * @param name The name of this instruction, as it will be written in Push programs.
  * @param stacks The stacks that this instruction is associated with - as in, don't include
  *               this instruction is those stacks aren't toggled on.
  */
abstract class AbstractInstruction(val name: String, val stacks: PushStackType*)
  extends InstructionDef {
}

object BooleanNot extends AbstractInstruction("boolean_not", BooleanStack) {
  override def apply(state: PushInterpreterState): PushInterpreterState = {
    val newBooleanStack = state.boolean.contents match {
      case firstBool :: rest => !firstBool :: rest
      case _ => List()
    }

    state.copy(boolean = PushStack[Boolean](newBooleanStack))
  }
}

object IntegerAdd extends AbstractInstruction("integer_add", IntStack) {
  override def apply(state: PushInterpreterState): PushInterpreterState = state.mapInt(_ + _)
}

object IntegerSub extends AbstractInstruction("integer_sub", IntStack) {
  override def apply(state: PushInterpreterState): PushInterpreterState = state.mapInt(_ - _)
}
