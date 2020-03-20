package gp

import io.evvo.island.population
import io.evvo.island.population.Scored
import org.scalatest.Matchers
import push4s.{PushBoolean, PushInt, PushString}

class IntAdd1Test extends org.scalatest.FunSpec with Matchers {
  val add1 = IntAdd1()
  describe("IntAdd1") {
    it("should not throw errors on empty programs") {
      add1.modify(Vector(Scored(Map(), Seq())))
    }

    it("should not throw errors on non-empty programs with no ints") {
      add1.modify(Vector(Scored(Map(), Seq(PushBoolean(false), PushString("")))))
    }

    it("should add 1 to an int if present") {
      val out = add1.modify(Vector(Scored(Map(), Seq(PushBoolean(false), PushInt(1), PushString("")))))
      out.head should contain(PushInt(2))
    }
  }
}
