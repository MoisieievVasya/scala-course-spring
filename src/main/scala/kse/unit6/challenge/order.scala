package kse.unit6.challenge

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object order:

  trait Order[T]:

    def compare(left: T, right: T): Int

  extension [V: Order as ord](elem: V)

    @targetName("greater than")
    infix def >(that: V): Boolean =
      ord.compare(elem, that) > 0

    @targetName("less than")
    infix def <(that: V): Boolean =
      ord.compare(elem, that) < 0

  given Order[Numeral] with

    def compare(left: Numeral, right: Numeral): Int =
      if left.toInt < right.toInt then -1
      else if left.toInt > right.toInt then 1
      else 0
