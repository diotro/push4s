package gp

import io.evvo.agent.CreatorFunction
import push4s.PushProgram

case class CreateEmptyProgram() extends CreatorFunction[PushProgram]("CreateEmptyProgram") {
  override protected def create(): Iterable[PushProgram] = Seq.fill(32)(Seq())
}
