package cmd

import io.evvo.agent.{CreatorFunction, ModifierFunction}
import io.evvo.builtin.deletors.DeleteDominated
import io.evvo.island.population.{Maximize, Objective, Scored}
import io.evvo.island.{EvvoIslandBuilder, StopAfter}
import push4s.PushProgram

import scala.concurrent.duration._
import scala.util.Random

object RunPush {
  def main(args: Array[String]): Unit = {
    val island = EvvoIslandBuilder[PushProgram]()
      .addCreator(PushCreator())
      .addModifier(PushModifier())
      .addDeletor(DeleteDominated())
      .addObjective(NullObjective())
      .build()

    island.runBlocking(StopAfter(1.seconds))
    println(island.currentParetoFrontier().toCsv())
  }
}

case class PushCreator() extends CreatorFunction[PushProgram]("create") {
  override def create(): Iterable[PushProgram] = Seq()
}

case class PushModifier() extends ModifierFunction[PushProgram]("modify") {
  override def modify(sols: IndexedSeq[Scored[PushProgram]]): Iterable[PushProgram] = {
    sols.map(_.solution)
  }
}

case class NullObjective() extends Objective[PushProgram]("null", Maximize) {
  override protected def objective(sol: PushProgram): Double = Random.nextDouble()
}
