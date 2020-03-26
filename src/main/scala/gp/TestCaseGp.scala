package gp

import io.evvo.agent.{CreatorFunction, DeletorFunction, ModifierFunction}
import io.evvo.builtin.deletors.DeleteDominated
import io.evvo.island.population.{Minimize, Objective}
import push4s.{Benchmark, PushElement, PushList, PushProgram}

// TODO make these conditional on the stacks that are included in the benchmark
// TODO add lexicase selection
case class BenchmarkGP(benchmark: Benchmark) {

  def creators(): Vector[CreatorFunction[PushProgram]] = {
    Vector(CreateEmptyProgram())
  }

  def mutators(): Vector[ModifierFunction[PushProgram]] = {
    Vector(
      AddZero(),
      IntAdd1(),
      IntSub1(),
      DoubleInt(),
      HalveInt(),
      AddEmptyString(),
      AddRandomCharacter(),
      RemoveRandomCharacter(),
      ChangeRandomCharacter(),
      AddRandomBoolean(),
      FlipRandomBoolean(),
      AddRandomInstruction(),
      AddRandomInstruction(),
      AddRandomInstruction(),
      AddRandomInstruction(),
      RemoveRandomElement(),
      MakeList(),
      RemoveList(),
      SwapOrder(),
      Crossover(),
      Append(),
    )
  }

  def deletors(): Vector[DeletorFunction[PushProgram]] = {
    Vector.fill(4)(DeleteDominated())
  }

  def objectives(): Vector[Objective[PushProgram]] = {
     benchmark.toObjectives.toVector :+ MinimizeLength
  }
}

object MinimizeLength extends Objective[PushProgram]("MinimizeLength", Minimize()) {
  override protected def objective(sol: PushProgram): Double = {
    sol.map(pushElementLength).iterator.sum
  }

  def pushElementLength(e: PushElement): Int = {
    e match {
      case l: PushList => l.contents.map(pushElementLength).iterator.sum
      case _ => 1
    }
  }
}
