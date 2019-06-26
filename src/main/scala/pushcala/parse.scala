package pushcala

object PushParser {
  /**
    * Parses the given push program (in string form)
    *
    * @param program She program to parse.
    * @return Some program, if it was parseable, or None, if it was not.
    */
  def parse(program: String): Option[PushProgram] = {
    // replace each paren with a " ( ", so we can split on spaces to get each atom out
    val atoms = program
      .replaceAllLiterally("(", " ( ")
      .replaceAllLiterally(")", " ) ")
      .split("\\s+")
      .filter(_ != "")
      .toList

    parseAtoms(atoms)
  }

  private def parseAtoms(atoms: List[String]): Option[PushProgram] = {
    println(s"atoms = ${atoms}")
    atoms match {
      case atom :: rest => atom match {
          // case 1: there is a left paren, so we have to parse a list right now
        case "(" =>
          def findMatchingParenIndex(index: Int, items: Seq[String], numOpen: Int): Option[Int] = {
            if (numOpen == 0) {
              return Some(index)
            }
            items match {
              case item :: more =>
                val newNumOpen = numOpen + (if (item == "(") 1 else if (item == ")") -1 else 0)
                findMatchingParenIndex(index + 1, more, newNumOpen)
              case _ => None
            }
          }

          val maybeMatchIndex = findMatchingParenIndex(0, rest, 1)
          maybeMatchIndex match {
            case Some(matchIndex) =>
              (parseAtoms(rest.take(matchIndex - 1)), parseAtoms(rest.drop(matchIndex))) match {
                case (Some(parsedFirst), Some(parsedRest)) => Some(PushList(parsedFirst) +: parsedRest)
                case _ => None
              }
            case None => None
          }

          // case 2: we can just parse the first atom and add it to the parsed rest of the
          // atoms, assuming all are parseable
        case _ => (parseAtom(atom), parseAtoms(rest)) match {
          case (Some(parsedAtom), Some(parsedRestAtoms)) =>
            println(parsedAtom, parsedRestAtoms, parsedAtom +: parsedRestAtoms)
            Some(parsedAtom +: parsedRestAtoms)
          case _ => None
        }
      }
      case _ => Some(List())
    }
  }

  private def parseAtom(atom: String): Option[PushAtom] = {
    atom match {
      case intStr if intStr.toIntOption.isDefined => Some(LiteralInt(intStr.toInt))
      case floatStr if floatStr.toFloatOption.isDefined => Some(LiteralFloat(floatStr.toFloat))
      case boolStr if boolStr.toBooleanOption.isDefined => Some(LiteralBoolean(boolStr.toBoolean))
      case str if str.startsWith("\"") && str.endsWith("\"") =>
        Some(LiteralString(str.substring(1, str.length - 1)))
      case instruction if Instruction.fromName(instruction).isDefined =>
        Instruction.fromName(instruction)
      case _ => None
    }
  }
}
