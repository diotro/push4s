package gp

import io.evvo.island.population.Scored
import org.scalatest.{FunSpec, Matchers}
import push4s.{PushBoolean, PushInt, PushString}

class AddRandomStringTest extends FunSpec with Matchers {
  describe("AddRandomString") {
    it("Should not throw errors on empty programs") {
      AddRandomString().modify(IndexedSeq(Scored(Map(), Seq())))
    }
    it("Should add a random string") {
      val out = AddRandomString().modify(IndexedSeq(Scored(Map(), Seq(PushBoolean(true)))))
      val out2 = AddRandomString().modify(out.map(Scored(Map(), _)).toVector)
      val ints = out2.head.collect {
        case x: PushString => x.value
      }
      // after running twice, should have two different integers in the solution
      ints.toVector.distinct should have size 2
    }
  }
}
