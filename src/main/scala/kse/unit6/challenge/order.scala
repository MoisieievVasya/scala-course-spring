package kse.unit6.challenge

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object order:

  trait Order[T]:
    def compare(left: T, right: T): Int

  extension [V: Order as ord](elem: V)

    infix def >(that: V): Boolean =
      ord.compare(elem, that) > 0

  given Order[Numeral] with

    def compare(left: Numeral, right: Numeral): Int =
      if left == right then 0
      else if left < right then -1
      else 1
