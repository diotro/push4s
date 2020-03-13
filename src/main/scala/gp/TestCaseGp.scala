package gp

import io.evvo.agent.{CreatorFunction, DeletorFunction, ModifierFunction}
import io.evvo.builtin.deletors.DeleteDominated
import io.evvo.island.population.Objective
import push4s.{Benchmark, PushProgram}

// TODO make these conditional on the stacks that are included in the benchmark
// TODO add lexicase selection
case class BenchmarkGP(benchmark: Benchmark) {

  def creators(): Vector[CreatorFunction[PushProgram]] = {
    Vector(CreateEmptyProgram())
  }

  def mutators(): Vector[ModifierFunction[PushProgram]] = {
    Vector(
      AddRandomInt(),
      IntAdd1(),
      IntSub1(),
      DoubleInt(),
      HalveInt(),
      AddRandomString(),
      AddRandomAsciiString(),
      AddRandomCharacter(),
      RemoveRandomCharacter(),
      ChangeRandomCharacter(),
      AddRandomBoolean(),
      FlipRandomBoolean(),
      AddRandomInstruction(),
      RemoveRandomElement(),
      Crossover(),
    )
  }

  def deletors(): Vector[DeletorFunction[PushProgram]] = {
    Vector(DeleteDominated())
  }

  def objectives(): Vector[Objective[PushProgram]] = {
    benchmark.toObjectives().toVector
  }
}
