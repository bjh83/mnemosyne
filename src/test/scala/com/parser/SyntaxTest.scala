package com.parser

import org.scalatest.FunSuite

class R_InstructionSuite extends FunSuite {
  
  test("sllv is the opcode, with all zero registers") {
    val instruction = R_Instruction("sllv", Zero, Zero, Zero)
    assert(instruction.toInt === Integer.parseInt("00000000000000000000000000000100", 2))
  }
  
  test("sllv is opcode, rd is a0, rs is v0, rt is v1") {
    val instruction = R_Instruction("sllv", A0, V0, V1)
    assert(instruction.toInt === Integer.parseInt("00000000010000110010000000000100", 2))
  }
  
  test("sltu is the opcode, rd is ra, rs is t0, rt is v0") {
    val instruction = R_Instruction("sltu", RA, T0, V0)
    assert(instruction.toInt === Integer.parseInt("00000001000000101111100000101011", 2))
  }
  
  test("invalid opcode") {
    val thrown = intercept[IllegalArgumentException] {
      R_Instruction("j", Zero, Zero, Zero).toInt
    }
  }
}

class R_ShiftInstructionSuite extends FunSuite {
  
  test("sll is instruction, registers are zero, immediate is zero") {
    val instruction = R_ShiftInstruction("sll", Zero, Zero, 0)
    assert(instruction.toInt === Integer.parseInt("00000000000000000000000000000000", 2))
  }
  
  test("srl is instruction, registers are zero, immediate is zero") {
    val instruction = R_ShiftInstruction("srl", Zero, Zero, 0)
    assert(instruction.toInt === Integer.parseInt("00000000000000000000000000000010", 2))
  }
  
  test("sll is instruction, rd is t0, rt is RA, immediate is 31 (all ones)") {
    val instruction = R_ShiftInstruction("sll", T0, RA, 31)
    assert(instruction.toInt === Integer.parseInt("00000000000111110100011111000000", 2))
  }
  
  test("invalid instruction") {
    val thrown = intercept[IllegalArgumentException] {
      R_ShiftInstruction("jal", Zero, Zero, 0).toInt
    }
  }
}

class I_InstructionSuite extends FunSuite {
  
  test("addi is the opcode, rt is zero, rs is zero, immediate is zero") {
    val instruction = I_Instruction("addi", Zero, Zero, 0)
    assert(instruction.toInt === Integer.parseInt("00100000000000000000000000000000", 2))
  }
  
  test("addi is the opcode, rt is v0, rs is v1, immediate is 0xffff (all ones)") {
    val instruction = I_Instruction("addi", V1, V0, 0xffff)
    assert(instruction.toInt === Integer.parseInt("00100000010000111111111111111111", 2))
  }
  
  test("swc1 is opcode, rt is t0, rs is ra, immediate is 0x00ff") {
    val instruction = I_Instruction("swc1", RA, T0, 0x00ff)
    assert(instruction.toInt === 0xe51f00ff) // 11100101000111110000000011111111
  }
  
  test("invalid opcode") {
    val thrown = intercept[IllegalArgumentException] {
      I_Instruction("sltu", Zero, Zero, 0).toInt
    }
  }
}

class J_InstructionSuite extends FunSuite {
  
  test("j is instruction, address is zero") {
    val instruction = J_Instruction("j", 0)
    assert(instruction.toInt === Integer.parseInt("00001000000000000000000000000000", 2))
  }
  
  test("jal is instruction, address is zero") {
    val instruction = J_Instruction("jal", 0)
    assert(instruction.toInt === Integer.parseInt("00001100000000000000000000000000", 2))
  }
  
  test("j is instruction, address is 67108863 (all ones)") {
    val instruction = J_Instruction("j", 67108863)
    assert(instruction.toInt === Integer.parseInt("00001011111111111111111111111111", 2))
  }
  
  test("invalid instruction") {
    val thrown = intercept[IllegalArgumentException] {
      J_Instruction("sltu", 0).toInt
    }
  }
}
