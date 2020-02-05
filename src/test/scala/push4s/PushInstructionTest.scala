package push4s

import org.scalatest.{FunSpec, Matchers}

class PushInstructionTest extends FunSpec with Matchers {
  describe("Instruction") {
    it("Should be creatable, if it exists in the instruction set") {
      PushInstruction.fromName("boolean_not") match {
        case Some(_) =>
        case None => fail()
      }
    }
    it("Should not be creatable, if there is no instruction with that name") {
      PushInstruction.fromName("hopefully_no_instruction_has_this_name") match {
        case Some(_) => fail()
        case None =>
      }
    }
  }

}
