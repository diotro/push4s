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
    // StringConcat, StringTake, StringLength, StringReverse,
    // StringParseToChars, StringContains, StringReplace,
  ).map(i => i.name -> i).toMap
}

/** Used to get Instructions */
object Instructions {
  def getDef(i: Instruction): InstructionDef = InstructionsMap.map(i.name)

  def containsDefinitionFor(name: String): Boolean = InstructionsMap.map.contains(name)
}


/** Defines an instruction. Instructions must have a name, which is used to reference them
  * from Push code, as well as the stacks they are associated with
  */
sealed trait InstructionDef extends (PushInterpreterState => PushInterpreterState) {
  val name: String
  val stack: PushStackType
}

/** Abstract class that all instructions should extend.
  *
  * @param name  The name of this instruction, as it will be written in Push programs.
  * @param stack The stack that this instruction is associated with - as in, don't include
  *              this instruction if that stack isn't in use.
  */
private case class Instruction(name: String,
                               stack: PushStackType,
                               func: PushInterpreterState => PushInterpreterState)
  extends InstructionDef {
  override def apply(state: PushInterpreterState): PushInterpreterState = func(state)
}


object BooleanAnd extends Instruction("boolean_and", BooleanStack, _.mapBoolean(_ && _))
object BooleanOr extends Instruction("boolean_or", BooleanStack, _.mapBoolean(_ || _))
object BooleanNot extends Instruction("boolean_not", BooleanStack, _.mapBoolean(!_))
object BooleanXor extends Instruction("boolean_xor", BooleanStack, _.mapBoolean(_ ^ _))
object BooleanFromInteger extends Instruction("boolean_frominteger", BooleanStack,
  _.mapIntToAny(i => LiteralBoolean(if (i == 0) false else true)))
object BooleanFromFloat extends Instruction("boolean_fromfloat", BooleanStack,
  _.mapFloatToAny(i => LiteralBoolean(if (i == 0f) false else true)))


object IntegerAdd extends Instruction("integer_add", IntStack, _.mapInt(_ + _))
object IntegerSub extends Instruction("integer_sub", IntStack, _.mapInt(_ - _))
object IntegerMult extends Instruction("integer_mult", IntStack, _.mapInt(_ * _))
object IntegerDiv extends Instruction("integer_div", IntStack, _.mapInt(_ / _))
object IntegerMod extends Instruction("integer_mod", IntStack, _.mapInt(_ % _))
object IntegerFromBoolean extends Instruction("integer_fromboolean", IntStack,
  _.mapBooleanToAny(b => LiteralInt(if (b) 1 else 0)))
object IntegerFromString extends Instruction("integer_fromstring", IntStack,
  state => Try {
    state.mapStringToAny(s => LiteralInt(s.toInt))
  }.getOrElse(state))
object IntegerInc extends Instruction("integer_inc", IntStack, _.mapInt(_ + 1))
object IntegerDec extends Instruction("integer_dec", IntStack, _.mapInt(_ - 1))


object FloatAdd extends Instruction("float_add", FloatStack, _.mapFloat(_ + _))
object FloatSub extends Instruction("float_sub", FloatStack, _.mapFloat(_ - _))
object FloatMult extends Instruction("float_mult", FloatStack, _.mapFloat(_ * _))
object FloatDiv extends Instruction("float_div", FloatStack, _.mapFloat(_ / _))
object FloatMod extends Instruction("float_mod", FloatStack, _.mapFloat(_ % _))
object FloatFromBoolean extends Instruction("float_fromboolean", FloatStack,
  _.mapBooleanToAny(b => LiteralFloat(if (b) 1f else 0f)))
object FloatFromString extends Instruction("float_fromstring", FloatStack,
  state => Try {
    state.mapStringToAny(s => LiteralFloat(s.toFloat))
  }.getOrElse(state))
object FloatInc extends Instruction("float_inc", FloatStack, _.mapFloat(_ + 1))
object FloatDec extends Instruction("float_dec", FloatStack, _.mapFloat(_ - 1))

//object StringConcat extends Instruction("string_concat", StringStack, _.mapString(_ ++ _))
//object StringTake extends Instruction("string_take", StringStack, state => {
//  val (maybeStr, newState) = state.popString()
//  maybeStr match {
//    case Some(s) => newState.popInt() match {
//      case (Some(i), finalState) => finalState.pushString(s.take(i))
//      case _ => state
//    }
//    case _ => state
//  }
//})
//object StringLength extends Instruction("string_length", StringStack, x => x)
//object StringReverse extends Instruction("string_reverse", StringStack, x => x)
//object StringParseToChars extends Instruction("string_parse_to_chars", StringStack,
//  state => {
//    state.popString() match {
//      case (Some(str), newState) =>
//        str.foldRight(newState)((char, stat) => stat.pushString(char.toString))
//    }
//  })
//object StringContains extends Instruction("string_contains", StringStack,
//  _.mapStringToAny((s1, s2) => LiteralBoolean(s1.contains(s2))))
//object StringReplace extends Instruction("string_replace", StringStack,
//  state => state.popString() match {
//    case (Some(str1), state1) =>
//      state1.popString() match {
//        case (Some(str2), state2) =>
//          state2.popString() match {
//            case (Some(str3), state3) => state3.pushString(str3.replace(str2, str1))
//            case _ => state
//          }
//        case _ => state
//      }
//    case _ => state
//  })

object CodeAppend extends Instruction("code_append", CodeStack, x => x)
object CodeAtom extends Instruction("code_atom", CodeStack, x => x)
object CodeCar extends Instruction("code_car", CodeStack, x => x)
object CodeCdr extends Instruction("code_cdr", CodeStack, x => x)
object CodeCons extends Instruction("code_cons", CodeStack, x => x)
object CodeDo extends Instruction("code_do", CodeStack, x => x)
object CodeDoStar extends Instruction("code_do*", CodeStack, x => x)
object CodeDoStarRange extends Instruction("code_do*range", CodeStack, x => x)
object CodeDoStarCount extends Instruction("code_do*count", CodeStack, x => x)
object CodeWrap extends Instruction("code_wrap", CodeStack, x => x)
object CodeList extends Instruction("code_list", CodeStack, x => x)
object CodeLength extends Instruction("code_length", CodeStack, x => x)
object CodeMap extends Instruction("code_map", CodeStack, x => x)
object CodeMember extends Instruction("code_member", CodeStack, x => x)
object CodeNth extends Instruction("code_nth", CodeStack, x => x)
object CodeNthcdr extends Instruction("code_nthcdr", CodeStack, x => x)
object CodeNull extends Instruction("code_null", CodeStack, x => x)
object CodeExtract extends Instruction("code_extract", CodeStack, x => x)
object CodeInsert extends Instruction("code_insert", CodeStack, x => x)
object CodeSubst extends Instruction("code_subst", CodeStack, x => x)
object CodeContains extends Instruction("code_contains", CodeStack, x => x)
object CodeContaining extends Instruction("code_containing", CodeStack, x => x)
object CodePosition extends Instruction("code_position", CodeStack, x => x)

object ExecDoStarRange extends Instruction("exec_do*range", ExecStack, x => x)
object ExecDoStarCount extends Instruction("exec_do*count", ExecStack, x => x)
