package com.parser

sealed abstract class Register {
  def toInt: Int
}

case object Zero extends Register {
  override def toInt = 0
}
case object AT extends Register {
  override def toInt = 1
}
case object V0 extends Register {
  override def toInt = 2
}
case object V1 extends Register {
  override def toInt = 3
}
case object A0 extends Register {
  override def toInt = 4
}
case object A1 extends Register {
  override def toInt = 5
}
case object A2 extends Register {
  override def toInt = 6
}
case object A3 extends Register {
  override def toInt = 7
}
case object T0 extends Register {
  override def toInt = 8
}
case object T1 extends Register {
  override def toInt = 9
}
case object T2 extends Register {
  override def toInt = 10
}
case object T3 extends Register {
  override def toInt = 11
}
case object T4 extends Register {
  override def toInt = 12
}
case object T5 extends Register {
  override def toInt = 13
}
case object T6 extends Register {
  override def toInt = 14
}
case object T7 extends Register {
  override def toInt = 15
}
case object S0 extends Register {
  override def toInt = 16
}
case object S1 extends Register {
  override def toInt = 17
}
case object S2 extends Register {
  override def toInt = 18
}
case object S3 extends Register {
  override def toInt = 19
}
case object S4 extends Register {
  override def toInt = 20
}
case object S5 extends Register {
  override def toInt = 21
}
case object S6 extends Register {
  override def toInt = 22
}
case object S7 extends Register {
  override def toInt = 23
}
case object T8 extends Register {
  override def toInt = 24
}
case object T9 extends Register {
  override def toInt = 25
}
case object K0 extends Register {
  override def toInt = 26
}
case object K1 extends Register {
  override def toInt = 27
}
case object GP extends Register {
  override def toInt = 28
}
case object SP extends Register {
  override def toInt = 29
}
case object FP extends Register {
  override def toInt = 30
}
case object RA extends Register {
  override def toInt = 31
}

sealed abstract class Instruction {
  def toInt: Int
}

case class R_Instruction(opcode: String, dest: Register, left: Register, right: Register) extends Instruction {
	override def toInt = left.toInt << 21 | right.toInt << 16 | dest.toInt << 6 | (opcode match {
	case "sllv" => 0x04
	case "srlv" => 0x06
	case "srav" => 0x07
	case "jr" => 0x08
	case "jalr" => 0x09
	case "syscall" => 0x0c
	case "break" => 0x0d
	case "mfhi" => 0x10
	case "mthi" => 0x11
	case "mflo" => 0x12
	case "mtlo" => 0x13
	case "mult" => 0x18
	case "multu" => 0x19
	case "div" => 0x1a
	case "divu" => 0x1b
	case "add" => 0x20
	case "addu" => 0x21
	case "sub" => 0x22
	case "subu" => 0x23
	case "and" => 0x24
	case "or" => 0x25
	case "xor" => 0x26
	case "nor" => 0x27
	case "slt" => 0x2a
	case "sltu" => 0x2b
  })
}

case class R_ShiftInstruction(opcode: String, dest: Register, source: Register, immed: Int) extends Instruction {
  override def toInt = source.toInt << 16 | dest.toInt << 11 | immed << 6 | (opcode match {
    case "sll" => 0x00
    case "srl" => 0x02
    case "sra" => 0x03
  })
}

case class I_Instruction(opcode: String, dest: Register, source: Register, immed: Int) extends Instruction {
  override def toInt = { 
    var rs = source.toInt
    var rt = dest.toInt
    (opcode match {
      case "bltz" => rt = 0x00; 0x01
      case "bgez" => rt = 0x01; 0x01
      case "beq" => 0x04
      case "bne" => 0x05
      case "blez" => rt = 0x00; 0x06
      case "bgtz" => rt = 0x00; 0x07
      case "addi" => 0x08
      case "addiu" => 0x09
      case "slti" => 0x0a
      case "sltiu" => 0x0b
      case "andi" => 0x0c
      case "ori" => 0x0d
      case "xort" => 0x0e
      case "lui" => 0x0f
      case "lb" => 0x20
      case "lh" => 0x21
      case "lw" => 0x23
      case "lbu" => 0x24
      case "lhu" => 0x25
      case "sb" => 0x28
      case "sh" => 0x29
      case "sw" => 0x2b
      case "lwc1" => 0x31
      case "swc1" => 0x39
    }) << 26 | rs << 21 | rt << 16 | immed
  }
}

case class J_Instruction(opcode: String, address: Int) extends Instruction {
  override def toInt = (opcode match {
    case "j" => 0x02
    case "jal" => 0x03
  }) << 26 | address
}
