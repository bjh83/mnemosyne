package com.parser

import scala.util.parsing.input.{CharSequenceReader, Reader}
import org.scalatest.{FunSuite, BeforeAndAfter}

class AssemblyParserSuite extends FunSuite with BeforeAndAfter {
  var parser: AssemblyParser = _

  implicit def stringToReader(in: String): Reader[Char] = new CharSequenceReader(in)

  before {
    parser = new AssemblyParser
  }

  /**
    * Tests the label parser
    */

  test("basic test label") {
    var result = parser.label("label:")
    assert(result successful)
    assert(result.get === "label")
    result = parser.label("something:")
    assert(result successful)
    assert(result.get === "something")
    result = parser.label("else:")
    assert(result successful)
    assert(result.get === "else")
  }

  test("label does not accept invalid labels") {
    var result = parser.label("label")
    assert(!result.successful)
    result = parser.label("$zero:")
    assert(!result.successful)
    result = parser.label("$v0:")
    assert(!result.successful)
    result = parser.label("add:")
    assert(!result.successful)
    result = parser.label("bne:")
    assert(!result.successful)
    result = parser.label("correct:")
    assert(result successful)
    assert( result.get === "correct")
  }

  test("label only accept unique labels") {
    var result = parser.label("something:")
    assert(result successful)
    assert(result.get === "something")
    result = parser.label("something:")
    assert(!result.successful)
    result = parser.label("something:")
    assert(!result.successful)
    result = parser.label("magic:")
    assert(result successful)
    assert(result.get === "magic")
    result = parser.label("magic:")
    assert(!result.successful)
  }

  /**
    * R_Instruction parsers
    */
  test("r_instruction3 parser") {
    var result = parser.r_instruction3("add $v0, $v1, $t0")
    assert(result successful)
    assert(result.get === R_Instruction("add", V0, V1, T0))
    result = parser.r_instruction3("xor $s4, $zero, $ra")
    assert(result successful)
    assert(result.get === R_Instruction("xor", S4, Zero, RA))
    result = parser.r_instruction3("div $v0, $v1, $t0")
    assert(!result.successful)
    result = parser.r_instruction3("add $v0 $v1 $t0")
    assert(!result.successful)
    result = parser.r_instruction3("add $v0, $v1")
    assert(!result.successful)
    result = parser.r_instruction3("add $v0, $v1, 100")
    assert(!result.successful)
  }
}
