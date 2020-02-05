package push4s

import scala.util.Try

/** This object holds the map of all instruction names to the actual instruction.
  * It is a var, so that each extender of AbstractInstruction can modify the map to add itself.
  * It has to be private, so that no one else can modify it - they can use Instructions to access
  * it's fields.
  */
private object InstructionsMap {
  var map: Map[String, InstructionDef] = List(
    BooleanAnd, BooleanOr, BooleanNot, BooleanXor, BooleanFromInteger, BooleanFromFloat,
    IntegerAdd, IntegerSub, IntegerMult, IntegerDiv, IntegerMod,
    IntegerFromBoolean, IntegerFromString, IntegerInc, IntegerDec,
    FloatAdd, FloatSub, FloatMult, FloatDiv, FloatMod,
    FloatFromBoolean, FloatFromString, FloatInc, FloatDec
     StringConcat, StringTake, StringLength, StringReverse,
     StringParseToChars, StringContains, StringReplace,
  ).map(i => i.name -> i).toMap
}

/** Used to get Instructions */
object Instructions {
  def getDef(i: Instruction): InstructionDef = InstructionsMap.map(i.name)

  def containsDefinitionFor(name: String): Boolean = InstructionsMap.map.contains(name)
}


/** Defines an instruction. Instructions must have a name, which is used to reference them
  * from Push code, as well as the stacks they are associated with
  *
  * @param name  The name of this instruction, as it will be written in Push programs.
  * @param stack The stack that this instruction is associated with - as in, don't include
  *              this instruction if that stack isn't in use.
  * @param func  The function to run when executing this instruction.
  */
case class InstructionDef(name: String,
                                  stack: PushStackType,
                                  func: PushInterpreterState => PushInterpreterState) {
  def apply(state: PushInterpreterState): PushInterpreterState = func(state)
}


object BooleanAnd extends InstructionDef("boolean_and", BooleanStack, _.mapBoolean(_ && _))
object BooleanOr extends InstructionDef("boolean_or", BooleanStack, _.mapBoolean(_ || _))
object BooleanNot extends InstructionDef("boolean_not", BooleanStack, _.mapBoolean(!_))
object BooleanXor extends InstructionDef("boolean_xor", BooleanStack, _.mapBoolean(_ ^ _))
object BooleanFromInteger extends InstructionDef("boolean_frominteger", BooleanStack,
  _.mapIntToAny(i => PushBoolean(if (i == 0) false else true)))
object BooleanFromFloat extends InstructionDef("boolean_fromfloat", BooleanStack,
  _.mapFloatToAny(i => PushBoolean(if (i == 0f) false else true)))


object IntegerAdd extends InstructionDef("integer_add", IntStack, _.mapInt(_ + _))
object IntegerSub extends InstructionDef("integer_sub", IntStack, _.mapInt(_ - _))
object IntegerMult extends InstructionDef("integer_mult", IntStack, _.mapInt(_ * _))
object IntegerDiv extends InstructionDef("integer_div", IntStack, _.mapInt(_ / _))
object IntegerMod extends InstructionDef("integer_mod", IntStack, _.mapInt(_ % _))
object IntegerFromBoolean extends InstructionDef("integer_fromboolean", IntStack,
  _.mapBooleanToAny(b => PushInt(if (b) 1 else 0)))
object IntegerFromString extends InstructionDef("integer_fromstring", IntStack,
  state => Try {
    state.mapStringToAny(s => PushInt(s.toInt))
  }.getOrElse(state))
object IntegerInc extends InstructionDef("integer_inc", IntStack, _.mapInt(_ + 1))
object IntegerDec extends InstructionDef("integer_dec", IntStack, _.mapInt(_ - 1))


object FloatAdd extends InstructionDef("float_add", FloatStack, _.mapFloat(_ + _))
object FloatSub extends InstructionDef("float_sub", FloatStack, _.mapFloat(_ - _))
object FloatMult extends InstructionDef("float_mult", FloatStack, _.mapFloat(_ * _))
object FloatDiv extends InstructionDef("float_div", FloatStack, _.mapFloat(_ / _))
object FloatMod extends InstructionDef("float_mod", FloatStack, _.mapFloat(_ % _))
object FloatFromBoolean extends InstructionDef("float_fromboolean", FloatStack,
  _.mapBooleanToAny(b => PushFloat(if (b) 1f else 0f)))
object FloatFromString extends InstructionDef("float_fromstring", FloatStack,
  state => Try {
    state.mapStringToAny(s => PushFloat(s.toFloat))
  }.getOrElse(state))
object FloatInc extends InstructionDef("float_inc", FloatStack, _.mapFloat(_ + 1))
object FloatDec extends InstructionDef("float_dec", FloatStack, _.mapFloat(_ - 1))

object StringConcat extends InstructionDef("string_concat", StringStack, _.mapString(_ ++ _))
object StringTake extends InstructionDef("string_take", StringStack, state => {
  val (maybeStr, newState) = state.popString()
  maybeStr match {
    case Some(s) => newState.popInt() match {
      case (Some(i), finalState) => finalState.pushString(s.take(i))
      case _ => state
    }
    case _ => state
  }
})
object StringLength extends InstructionDef("string_length", StringStack, x => x)
object StringReverse extends InstructionDef("string_reverse", StringStack, x => x)
object StringParseToChars extends InstructionDef("string_parse_to_chars", StringStack,
  state => {
    state.popString() match {
      case (Some(str), newState) =>
        str.foldRight(newState)((char, stat) => stat.pushString(char.toString))
    }
  })
object StringContains extends InstructionDef("string_contains", StringStack,
  _.mapStringToAny((s1, s2) => PushBoolean(s1.contains(s2))))
object StringReplace extends InstructionDef("string_replace", StringStack,
  state => state.popString() match {
    case (Some(str1), state1) =>
      state1.popString() match {
        case (Some(str2), state2) =>
          state2.popString() match {
            case (Some(str3), state3) => state3.pushString(str3.replace(str2, str1))
            case _ => state
          }
        case _ => state
      }
    case _ => state
  })

object CodeAppend extends InstructionDef("code_append", CodeStack, x => x)
object CodeAtom extends InstructionDef("code_atom", CodeStack, x => x)
object CodeCar extends InstructionDef("code_car", CodeStack, x => x)
object CodeCdr extends InstructionDef("code_cdr", CodeStack, x => x)
object CodeCons extends InstructionDef("code_cons", CodeStack, x => x)
object CodeDo extends InstructionDef("code_do", CodeStack, x => x)
object CodeDoStar extends InstructionDef("code_do*", CodeStack, x => x)
object CodeDoStarRange extends InstructionDef("code_do*range", CodeStack, x => x)
object CodeDoStarCount extends InstructionDef("code_do*count", CodeStack, x => x)
object CodeWrap extends InstructionDef("code_wrap", CodeStack, x => x)
object CodeList extends InstructionDef("code_list", CodeStack, x => x)
object CodeLength extends InstructionDef("code_length", CodeStack, x => x)
object CodeMap extends InstructionDef("code_map", CodeStack, x => x)
object CodeMember extends InstructionDef("code_member", CodeStack, x => x)
object CodeNth extends InstructionDef("code_nth", CodeStack, x => x)
object CodeNthcdr extends InstructionDef("code_nthcdr", CodeStack, x => x)
object CodeNull extends InstructionDef("code_null", CodeStack, x => x)
object CodeExtract extends InstructionDef("code_extract", CodeStack, x => x)
object CodeInsert extends InstructionDef("code_insert", CodeStack, x => x)
object CodeSubst extends InstructionDef("code_subst", CodeStack, x => x)
object CodeContains extends InstructionDef("code_contains", CodeStack, x => x)
object CodeContaining extends InstructionDef("code_containing", CodeStack, x => x)
object CodePosition extends InstructionDef("code_position", CodeStack, x => x)

object ExecDoStarRange extends InstructionDef("exec_do*range", ExecStack, x => x)
object ExecDoStarCount extends InstructionDef("exec_do*count", ExecStack, x => x)
