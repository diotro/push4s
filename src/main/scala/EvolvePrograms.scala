import io.evvo.agent.{CreatorFunction, DeletorFunction, MutatorFunction}
import io.evvo.agent.defaults.DeleteDominated
import io.evvo.builtin.deletors.DeleteDominated
import io.evvo.island.EvvoIslandBuilder
import io.evvo.island.population.{Minimize, Objective, Scored}
import push4s.{PushInterpreter, PushProgram}

object EvolvePrograms {

  def main(args: Array[String]): Unit = {

    val builder = EvvoIslandBuilder[PushProgram]()
      .addObjective(Returns5())
      .addCreator(CreateRandomPushProgram(length=16))
      .addModifier(RandomMutation())
      .addDeletor(DeleteDominated())
  }
}

case class Returns5() extends Objective[PushProgram]("Returns5", Minimize) {

  override protected def objective(sol: PushProgram): Double = {
    Math.abs(PushInterpreter.runProgram(sol).popInt()._1.getOrElse(0) - 5)
  }
}


case class CreateRandomPushProgram(length: Int) extends CreatorFunction[PushProgram]("RandomPush") {
  // TODO: We seem to prefer Creator as a name to CreatorFunction
  override protected def create(): Iterable[PushProgram] = ???
}


case class RandomMutation() extends MutatorFunction[PushProgram]("RandomMutate") {
  override protected def mutate(sol: PushProgram): PushProgram = ???
}
