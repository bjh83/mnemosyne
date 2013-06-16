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

  test("r_shiftInstruction3 parser") {
    var result = parser.r_shiftInstruction3("sll $v0, $t2, 4")
    assert(result successful)
    assert(result.get === R_ShiftInstruction("sll", V0, T2, 4))
    result = parser.r_shiftInstruction3("sra $at, $zero, 0")
    assert(result successful)
    assert(result.get === R_ShiftInstruction("sra", AT, Zero, 0))
    result = parser.r_shiftInstruction3("sll $at $zero, 1")
    assert(!result.successful)
    result = parser.r_shiftInstruction3("sll 1, 2, 3")
    assert(!result.successful)
    result = parser.r_shiftInstruction3("sll $v0, $t2, $t0")
    assert(!result.successful)
  }

  test("r_instruction2 parser") {
    var result = parser.r_instruction2("div $t0, $t7")
    assert(result successful)
    assert(result.get === R_Instruction("div", Zero, T0, T7))
    result = parser.r_instruction2("multu $s3, $s5")
    assert(result successful)
    assert(result.get === R_Instruction("multu", Zero, S3, S5))
    result = parser.r_instruction2("div $t0, $t7, $s3")
    assert(!result.successful)
    result = parser.r_instruction2("div $t0, $t7, 100")
    assert(!result.successful)
    result = parser.r_instruction2("add $v0, $s1")
    assert(!result.successful)
  }
  
  test("jalr parser") {
    var result = parser.jalr("jalr $t0, $t9")
    assert(result successful)
    assert(result.get === R_Instruction("jalr", T0, T9, Zero))
    result = parser.jalr("jalr $v0, $s1")
    assert(result successful)
    assert(result.get === R_Instruction("jalr", V0, S1, Zero))
    result = parser.jalr("jalr $t0, $t9, $v0")
    assert(!result.successful)
    result = parser.jalr("add $v0, $s1")
    assert(!result.successful)
  }

  test("r_instruction1_source parser") {
    val r_parser = parser.r_instruction1_source
    var result = r_parser("jr $fp")
    assert(result successful)
    assert(result.get === R_Instruction("jr", Zero, Zero, FP))
    result = r_parser("mtlo $t0")
    assert(result successful)
    assert(result.get === R_Instruction("mtlo", Zero, Zero, T0))
    result = r_parser("add $t0")
    assert(!result.successful)
    result = r_parser("jr $t0, $t9")
    assert(!result.successful)
  }

  test("r_instruction1_dest parser") {
    val r_parser = parser.r_instruction1_dest
    var result = r_parser("mflo $gp")
    assert(result successful)
    assert(result.get === R_Instruction("mflo", GP, Zero, Zero))
    result = r_parser("mfhi $t0")
    assert(result successful)
    assert(result.get === R_Instruction("mfhi", T0, Zero, Zero))
    result = r_parser("and $t0")
    assert(!result.successful)
    result = r_parser("mflo $gp, $t0")
    assert(!result.successful)
  }

  test("r_instruction0 parser") {
    var result = parser.r_instruction0("break")
    assert(result successful)
    assert(result.get === R_Instruction("break", Zero, Zero, Zero))
    result = parser.r_instruction0("syscall")
    assert(result successful)
    assert(result.get === R_Instruction("syscall", Zero, Zero, Zero))
    result = parser.r_instruction0("xor")
    assert(!result.successful)
    result = parser.r_instruction0("break $t0")
    assert(!result.successful)
  }

  /**
    * I_Instruction parsers
    */

  test("i_instruction3 parser") {
    var result = parser.i_instruction3("addi $v1, $t0, 400")
    assert(result successful)
    assert(result.get === I_Instruction("addi", V1, T0, 400))
    result = parser.i_instruction3("ori $a3, $k0, 5")
    assert(result successful)
    assert(result.get === I_Instruction("ori", A3, K0, 5))
    result = parser.i_instruction3("add $v1, $t0, 400")
    assert(!result.successful)
    result = parser.i_instruction3("addi $v1, $t0, $k0")
    assert(!result.successful)
  }

  test("i_load_store_instruction parser") {
    val i_parser = parser.i_load_store_instruction
    var result = i_parser("lb $t0, 0($k1)")
    assert(result successful)
    assert(result.get === I_Instruction("lb", T0, K1, 0))
    result = i_parser("sw $s0, 321($s4)")
    assert(result successful)
    assert(result.get === I_Instruction("sw", S0, S4, 321))
    result = i_parser("lb $t0, $k1, 0")
    assert(!result.successful)
    result = i_parser("lb $t0, 0(0)")
    assert(!result.successful)
    result = i_parser("add $t0, 0($k1)")
    assert(!result.successful)
  }

  test("lui parser") {
    var result = parser.lui("lui $t0, 1444")
    assert(result successful)
    assert(result.get === I_Instruction("lui", T0, Zero, 1444))
    result = parser.lui("lui $sp, 50000")
    assert(result successful)
    assert(result.get === I_Instruction("lui", SP, Zero, 50000))
    result = parser.lui("ori $t0, 1444")
    assert(!result.successful)
    result = parser.lui("lui $t0, $sp")
    assert(!result.successful)
  }

  test("i_compareInstruction3 parser") {
    val i_parser = parser.i_compareInstruction3
    var result = i_parser("beq $t0, $t3, label")
    assert(!result.successful)
    result = parser.label("label:")
    assert(result successful)
    result = i_parser("beq $t0, $t3, label")
    assert(result successful)
    assert(result.get === I_Instruction("beq", T3, T0, 0))
    result = i_parser("bne $s1, $t2, label")
    assert(result successful)
    assert(result.get === I_Instruction("bne", T2, S1, 0))
    result = i_parser("bne $s1, $t2, label2")
    assert(!result.successful)
    result = parser.label("label2:")
    assert(result successful)
    result = i_parser("bne $s1, $t2, label2")
    assert(result successful)
    assert(result.get === I_Instruction("bne", T2, S1, 0))
    result = i_parser("bltz $s1, $t2, label2")
    assert(!result.successful)
  }

  test("i_compareInstruction2 parser") {
    val i_parser = parser.i_compareInstruction2
    var result = i_parser("bgez $s0, label")
    assert(!result.successful)
    result = i_parser("bltz $v0, label")
    assert(!result.successful)
    result = parser.label("label:")
    assert(result successful)
    result = i_parser("bltz $v0, label")
    assert(result successful)
    assert(result.get === I_Instruction("bltz", Zero, V0, 0))
    result = i_parser("bgez $s0, label")
    assert(result successful)
    assert(result.get === I_Instruction("bgez", Zero, S0, 0))
    result = i_parser("bltz $s0, label2")
    assert(!result.successful)
    result = parser.label("label2:")
    assert(result successful)
    result = i_parser("bltz $s0, label2")
    assert(result successful)
    assert(result.get === I_Instruction("bltz", Zero, S0, 0))
    result = i_parser("beq $s0, label2")
    assert(!result.successful)
  }

  /**
    * J_Compare instruction parsers
    */

  test("j_instruction parser") {
    var result = parser.j_instruction("j label")
    assert(!result.successful)
    result = parser.label("label:")
    assert(result successful)
    result = parser.j_instruction("j label")
    assert(result successful)
    assert(result.get === J_Instruction("j", 0))
  }
}
