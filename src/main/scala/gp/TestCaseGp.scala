package gp

import io.evvo.agent.{CreatorFunction, DeletorFunction, ModifierFunction}
import io.evvo.builtin.deletors.DeleteDominated
import push4s.{Benchmark, PushProgram}

// TODO make these conditional on the stacks that are included in the benchmark
// TODO add lexicase selection
case class BenchmarkGP(benchmark: Benchmark) {

  def creators(): Seq[CreatorFunction[PushProgram]] = {
    Seq(CreateEmptyProgram())
  }

  def mutators(): Seq[ModifierFunction[PushProgram]] = {
    Seq(
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

  def deletors(): Seq[DeletorFunction[PushProgram]] = {
    Seq(DeleteDominated())
  }
}
