package gp

import io.evvo.island.population
import org.scalatest.{FunSpec, Matchers}
import push4s.PushInt

class RemoveRandomElementTest extends FunSpec with Matchers {
  val r = RemoveRandomElement()
  describe("RemoveRandomElement") {
    it("should not throw exceptions on empty programs") {
      r.modify(Vector(population.Scored(Map(), Seq())))
    }

    it("should remove a random element") {
      val out = r.modify(
        Vector.fill(100)(
          population.Scored(
            Map(),
            Seq(PushInt(1), PushInt(2), PushInt(3), PushInt(4), PushInt(5))
          )
        )
      ).toVector.distinct
      out should have size 5
      out should contain(Seq(PushInt(2), PushInt(3), PushInt(4), PushInt(5)))
      out should contain(Seq(PushInt(1), PushInt(3), PushInt(4), PushInt(5)))
      out should contain(Seq(PushInt(1), PushInt(2), PushInt(4), PushInt(5)))
      out should contain(Seq(PushInt(1), PushInt(2), PushInt(3), PushInt(5)))
      out should contain(Seq(PushInt(1), PushInt(2), PushInt(3), PushInt(4)))

    }
  }
}
