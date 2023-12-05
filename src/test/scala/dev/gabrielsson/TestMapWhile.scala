package dev.gabrielsson

import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class TestMapWhile extends AnyFlatSpec{

  it should "do that than" in {

    val mutableMap = mutable.Map[Int, Int]().withDefaultValue(0)
    mutableMap(1) += 10
    do {
      mutableMap(1) -= 1
    } while (mutableMap(1) > 0)
    println(mutableMap)
  }

}
