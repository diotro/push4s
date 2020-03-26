package gp

import io.evvo.island.population.Scored
import org.scalatest.{FunSpec, Matchers}
import push4s.{PushBoolean, PushInt, PushString}

class AddEmptyStringTest extends FunSpec with Matchers {
  describe("AddRandomString") {
    it("Should not throw errors on empty programs") {
      AddEmptyString().modify(IndexedSeq(Scored(Map(), Seq())))
    }
    it("Should add a random string") {
      val out = AddEmptyString().modify(IndexedSeq(Scored(Map(), Seq(PushBoolean(true)))))
      val out2 = AddEmptyString().modify(out.map(Scored(Map(), _)).toVector)
      val strs = out2.head.collect {
        case x: PushString => x.value
      }
      // after running twice, should have two different integers in the solution
      strs.toVector should contain(PushString(""))
    }
  }
}
