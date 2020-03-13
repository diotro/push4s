package gp

import io.evvo.agent.{CrossoverFunction, MutatorFunction}
import push4s._

// TODO these are severely untested

/**
  * Parent class for mutators that add elements in a random place.
  */
abstract class RandomAdder(override val name: String)
    extends MutatorFunction[PushProgram](name) {
  override protected def mutate(sol: PushProgram): PushProgram = {
    val insertionPoint = util.Random.nextInt(math.max(sol.length, 1))
    val newValue = valueToAdd()
    val (left, right) = sol.splitAt(insertionPoint)
    left ++ (newValue +: right)
  }

  protected def valueToAdd(): PushElement
}

case class AddRandomInt() extends RandomAdder("AddRandomInt") {
  override protected def valueToAdd(): PushElement = {
    PushInt(util.Random.nextInt())
  }
}

abstract class IntMapper(override val name: String, func: Int => Int)
    extends MutatorFunction[PushProgram](name) {
  override protected def mutate(sol: PushProgram): PushProgram = {
    val pushInts = sol.zipWithIndex.collect {
      case (int: PushInt, index: Int) => (index, int)
    }
    if (pushInts.isEmpty) {
      sol
    } else {
      val chosen = pushInts(util.Random.nextInt(math.max(1, pushInts.length)))
      val index = chosen._1
      val prevInt = chosen._2.value
      sol.updated(index, PushInt(func(prevInt)))
    }
  }
}

case class IntAdd1() extends IntMapper("IntAdd1", _ + 1)
case class IntSub1() extends IntMapper("IntSub1", _ - 1)
case class DoubleInt() extends IntMapper("IntDoubler", _ * 2)
case class HalveInt() extends IntMapper("IntHalver", _ / 2)

abstract class StringMapper(override val name: String)
    extends MutatorFunction[PushProgram](name) {
  override protected def mutate(sol: PushProgram): PushProgram = {
    val pushStrings = sol.zipWithIndex.collect {
      case (str: PushString, index: Int) => (index, str)
    }

    if (pushStrings.isEmpty) {
      sol
    } else {
      val chosen = pushStrings(util.Random.nextInt(math.max(1, pushStrings.length)))
      val index = chosen._1
      val prevVal = chosen._2.value
      sol.updated(index, PushString(func(prevVal)))
    }
  }

  protected def func(s: String): String
}

case class AddRandomString(maxLength: Int = 25)
    extends RandomAdder("AddRandomString") {
  override protected def valueToAdd(): PushElement = {
    val length = util.Random.nextInt(maxLength)
    PushString(util.Random.nextString(length))
  }
}

case class AddRandomCharacter() extends StringMapper("AddRandomCharacter") {
  override def func(s: String): String = {
    val insertionPoint = util.Random.nextInt(math.max(1, s.length))
    val (left, right) = s.splitAt(insertionPoint)
    left ++ (util.Random.nextPrintableChar() +: right)
  }
}

case class AddRandomAsciiString(maxLength: Int = 50)
    extends StringMapper("AddRandomAsciiString") {
  override protected def func(s: String): String = {
    val sb = new StringBuilder()
    (1 to maxLength).foreach(_ => sb.addOne(util.Random.nextInt(256).toChar))
    sb.toString()
  }
}

case class RemoveRandomCharacter()
    extends StringMapper("RemoveRandomCharacter") {
  override def func(s: String): String = {
    val deletionPoint = util.Random.nextInt(math.max(1, s.length - 1))
    val (left, right) = s.splitAt(deletionPoint)
    left ++ right.tail
  }
}

case class ChangeRandomCharacter()
    extends StringMapper("ChangeRandomCharacter") {
  override def func(s: String): String = {
    val insertionPoint = util.Random.nextInt(math.max(1, s.length - 1))
    val (left, right) = s.splitAt(insertionPoint)
    left ++ (util.Random.nextPrintableChar() +: right.tail)
  }
}

case class AddRandomBoolean() extends RandomAdder("AddRandomBoolean") {
  override protected def valueToAdd(): PushElement =
    PushBoolean(util.Random.nextBoolean())
}

case class FlipRandomBoolean()
    extends MutatorFunction[PushProgram]("FlipRandomBoolean") {
  override protected def mutate(sol: PushProgram): PushProgram = {
    val bools = sol.zipWithIndex.collect {
      case (b: PushBoolean, index: Int) => (index, b)
    }

    val chosen = bools(util.Random.nextInt(math.max(1, bools.length)))
    val index = chosen._1
    val prevVal = chosen._2.value
    sol.updated(index, PushBoolean(!prevVal))
  }
}

case class AddRandomInstruction() extends RandomAdder("AddRandomInstruction") {
  override protected def valueToAdd(): PushElement = {
    PushInstruction(Instructions.randomInstruction().name)
  }
}

case class RemoveRandomElement()
    extends MutatorFunction[PushProgram]("RemoveRandomElement") {
  override protected def mutate(sol: PushProgram): PushProgram = {
    val insertionPoint = util.Random.nextInt(math.max(sol.length - 1, 1))
    val (left, right) = sol.splitAt(insertionPoint)
    left ++ right.tail
  }
}

case class Crossover() extends CrossoverFunction[PushProgram]("Crossover") {
  override protected def crossover(sol1: PushProgram,
                                   sol2: PushProgram): PushProgram = {
    val crossoverPoint1 = util.Random.nextInt(math.max(sol1.length, 1))
    val crossoverPoint2 = util.Random.nextInt(math.max(sol2.length, 1))
    sol1.take(crossoverPoint1) ++ sol2.takeRight(crossoverPoint2)
  }
}
