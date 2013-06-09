package com.parser

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.HashMap

class AssemblyParser extends RegexParsers {
  
  private var lineCount = 0
  private var labelTable = new HashMap[String, Int]
  
  val identifier: Parser[String] = """\w+""".r
  val number: Parser[Int] = """\d+""".r ^^ { _.toInt }
  val checkInstruction: Parser[String] = ("add" | "addu" | "and" | "nor" | "or" | "sllv" | "slt" |
      "sltu" | "srav" | "srlv" | "sub" | "subu" | "xor" | "sll" | "sra" | "srl" | "div" | "divu" |
      "mult" | "multu" | "jalr" | "jr" | "mtlo" | "mthi" | "mflo" | "mfhi" | "break" | "syscall" |
      "addi" | "addiu" | "andi" | "lb" | "lbu" | "lh" | "lhu" | "lw" | "lwc1" | "ori" | "sb" | "slti" |
      "sltiu" | "sh" | "sw" | "swc1" | "xori" | "lui" | "beq" | "bne" | "bgez" | "bgtz" | "blez" |
      "bltz" | "j" | "jal")
      
  val label: Parser[String] = not(checkInstruction) ~> (identifier <~ ":") ^^ { 
    label => if(labelTable contains label) {
      scala.sys.error("Label was already used")
    } else {
      labelTable(label) = lineCount
      label
    }
  }
  
  val r_instruction3: Parser[R_Instruction] = ("add" | "addu" | "and" | "nor" | "or" | "sllv" | "slt" |
      "sltu" | "srav" | "srlv" | "sub" | "subu" | "xor") ~ register ~ ("," ~> register) ~ ("," ~> register) ^^ {
    case opcode ~ dest ~ left ~ right => R_Instruction(opcode, dest, left, right)
  }
  
  val r_shiftInstruction3: Parser[R_ShiftInstruction] = ("sll" | "sra" | "srl") ~ register ~ ("," ~> register) ~ ("," ~> number) ^^ {
    case opcode ~ dest ~ source ~ immed => R_ShiftInstruction(opcode, dest, source, immed)
  }
  
  val r_instruction2: Parser[R_Instruction] = ("div" | "divu" | "mult" | "multu") ~ register ~ ("," ~> register) ^^ {
    case opcode ~ dest ~ right => R_Instruction(opcode, dest, Zero, right)
  }
  
  val jalr: Parser[R_Instruction] = "jalr" ~ register ~ ("," ~> register) ^^ {
    case opcode ~ dest ~ left => R_Instruction(opcode, dest, left, Zero)
  }
  
  val r_instruction1_source: Parser[R_Instruction] = ("jr" | "mtlo" | "mthi") ~ register ^^ {
    case opcode ~ source => R_Instruction(opcode, Zero, source, Zero)
  }
  
  val r_instruction1_dest: Parser[R_Instruction] = ("mflo" | "mfhi") ~ register ^^ {
    case opcode ~ dest => R_Instruction(opcode, dest, Zero, Zero)
  }
  
  val r_instruction0: Parser[R_Instruction] = ("break" | "syscall") ^^ {
    opcode => R_Instruction(opcode, Zero, Zero, Zero)
  }
  
  val r_instruction: Parser[Instruction] = (r_instruction3 | r_shiftInstruction3 | r_instruction2 | jalr |
      r_instruction1_source | r_instruction1_dest | r_instruction0)
      
  val i_instruction3: Parser[I_Instruction] = ("addi" | "addiu" | "andi" | "lb" | "lbu" | "lh" | "lhu" | "lw" | "lwc1" |
      "ori" | "sb" | "slti" | "sltiu" | "sh" | "sw" | "swc1" | "xori") ~ register ~ ("," ~> register) ~ ("," ~> number) ^^ {
    case opcode ~ dest ~ source ~ number => I_Instruction(opcode, dest, source, number)
  }
  
  val lui: Parser[I_Instruction] = "lui" ~ register ~ ("," ~> number) ^^ {
    case opcode ~ dest ~ immed => I_Instruction(opcode, dest, Zero, immed)
  }
  
  val i_compareInstruction3: Parser[I_Instruction] = ("beq" | "bne") ~ register ~ ("," ~> register) ~ ("," ~> identifier) ^^ {
    case opcode ~ left ~ right ~ label => I_Instruction(opcode, left, right, labelTable(label))
  }
  
  val i_compareInstruction2: Parser[I_Instruction] = ("bgez" | "bgtz" | "blez" | "bltz") ~ register ~ ("," ~> identifier) ^^ {
    case opcode ~ left ~ label => I_Instruction(opcode, left, Zero, labelTable(label))
  }
  
  val i_instruction: Parser[Instruction] = i_instruction3 | lui | i_compareInstruction3 | i_compareInstruction2
  
  val j_instruction: Parser[Instruction] = ("j" | "jal") ~ identifier ^^ {
    case opcode ~ label => J_Instruction(opcode, labelTable(label))
  }
  
  val line: Parser[Instruction] = label.? ~ (r_instruction | i_instruction | j_instruction) ^^ {
    case label ~ instruction => { lineCount += 1; instruction }
  }
  
  val text: Parser[List[Instruction]] = line*
  
  val zero: Parser[Register] = "$zero" ^^ { _ => Zero }
  val at: Parser[Register] = "$at" ^^ { _ => At }
  val v0: Parser[Register] = "$v0" ^^ { _ => V0 }
  val v1: Parser[Register] = "$v1" ^^ { _ => V1 }
  val a0: Parser[Register] = "$a0" ^^ { _ => A0 }
  val a1: Parser[Register] = "$a1" ^^ { _ => A1 }
  val a2: Parser[Register] = "$a2" ^^ { _ => A2 }
  val a3: Parser[Register] = "$a3" ^^ { _ => A3 }
  val t0: Parser[Register] = "$t0" ^^ { _ => T0 }
  val t1: Parser[Register] = "$t1" ^^ { _ => T1 }
  val t2: Parser[Register] = "$t2" ^^ { _ => T2 }
  val t3: Parser[Register] = "$t3" ^^ { _ => T3 }
  val t4: Parser[Register] = "$t4" ^^ { _ => T4 }
  val t5: Parser[Register] = "$t5" ^^ { _ => T5 }
  val t6: Parser[Register] = "$t6" ^^ { _ => T6 }
  val t7: Parser[Register] = "$t7" ^^ { _ => T7 }
  val s0: Parser[Register] = "$s0" ^^ { _ => S0 }
  val s1: Parser[Register] = "$s1" ^^ { _ => S1 }
  val s2: Parser[Register] = "$s2" ^^ { _ => S2 }
  val s3: Parser[Register] = "$s3" ^^ { _ => S3 }
  val s4: Parser[Register] = "$s4" ^^ { _ => S4 }
  val s5: Parser[Register] = "$s5" ^^ { _ => S5 }
  val s6: Parser[Register] = "$s6" ^^ { _ => S6 }
  val s7: Parser[Register] = "$s7" ^^ { _ => S7 }
  val t8: Parser[Register] = "$t8" ^^ { _ => T8 }
  val t9: Parser[Register] = "$t9" ^^ { _ => T9 }
  val k0: Parser[Register] = "$k0" ^^ { _ => K0 }
  val k1: Parser[Register] = "$k1" ^^ { _ => K1 }
  val gp: Parser[Register] = "$gp" ^^ { _ => GP }
  val sp: Parser[Register] = "$sp" ^^ { _ => SP }
  val fp: Parser[Register] = "$fp" ^^ { _ => FP }
  val ra: Parser[Register] = "$ra" ^^ { _ => RA }
  val register: Parser[Register] = (zero | at | v0 | v1 | a0 | a1 | a2 | a3 | t0 | t1 | t2 | t3 | t4 | t5 | t6 |
      t7 | s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 | t8 | t9 | k0 | k1 | gp | sp | fp | ra)
      
  def apply(input: String): List[Instruction] = parseAll(text, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}
