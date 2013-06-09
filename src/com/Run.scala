package com

import com.parser.AssemblyParser
import scala.io.Source

object Run {

  def main(args: Array[String]) {
    if(args.length != 1) {
      scala.sys.error("Wrong number of argument(s)")
    }
    val parser = new AssemblyParser
    println(parser(Source.fromFile(args(0)).mkString))
  }

}