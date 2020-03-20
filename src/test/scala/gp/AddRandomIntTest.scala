package gp

import io.evvo.island.population.Scored
import org.scalatest.{FunSpec, Matchers}
import push4s.{PushBoolean, PushInt, PushProgram}

class AddRandomIntTest extends FunSpec with Matchers {
  describe("AddRandomInt") {
    it("Should not throw errors on empty programs") {
        AddRandomInt().modify(IndexedSeq(Scored(Map(), Seq())))
    }
    it("Should add a random int") {
      val out = AddRandomInt().modify(IndexedSeq(Scored(Map(), Seq(PushBoolean(true)))))
      val out2 = AddRandomInt().modify(out.map(Scored(Map(), _)).toVector)
      val ints = out2.head.collect {
        case x: PushInt => x.value
      }
      // after running twice, should have two different integers in the solution
      ints.toVector.distinct should have size 2
    }
  }
}
