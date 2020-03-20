package gp

import io.evvo.island.population.Scored
import org.scalatest.{FunSpec, Matchers}
import push4s.{PushBoolean, PushInt}

class FlipRandomBooleanTest extends FunSpec with Matchers {
  val flip = FlipRandomBoolean()
  describe("FlipRandomBoolean") {
    it ("should not throw errors on empty solutions") {
      flip.modify(Vector(Scored(Map(), Seq())))
    }
    it ("should not throw errors on non-mempty solutions with no booleans") {
      flip.modify(Vector(Scored(Map(), Seq(PushInt(1)))))
    }
    it ("should flip a boolean") {
      val out = flip.modify(Vector(Scored(Map(), Seq(PushInt(1), PushBoolean(false)))))
      out.head should contain(PushBoolean(true))
    }
  }
}
