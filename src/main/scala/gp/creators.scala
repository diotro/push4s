package gp

import io.evvo.agent.CreatorFunction
import push4s.{PushBoolean, PushInt, PushProgram, PushString}

case class CreateEmptyProgram() extends CreatorFunction[PushProgram]("CreateEmptyProgram") {
  override protected def create(): Iterable[PushProgram] = Seq.fill(32)(Seq(PushInt(0), PushString(""), PushBoolean(true)))
}
