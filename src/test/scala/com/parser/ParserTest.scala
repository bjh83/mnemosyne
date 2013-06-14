package com.parser

import org.scalatest.FunSuite

class AssemblyParserSuite extends FunSuite {

  test("basic test label") {
    val parser = new AssemblyParser
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

  test("label only accept unique labels") {
    val parser = new AssemblyParser
    var result = parser.label("something:")
    assert(result successful)
    assert(result === "something")
    result = parser.label("something:")
    assert(!result.successful)
    result = parser.label("something:")
    assert(!result.successful)
    result = parser.label("magic:")
    assert(result successful)
    assert(result === "magic")
    result = parser.label("magic:")
    assert(!result.successful)
  }
}
