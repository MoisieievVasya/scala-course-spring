package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val increment: Number => Number =
    value => value + 1

  val decrement: Number => Number =
    value => value - 1

  val isZero: Number => Boolean =
    value => value == 0

  val isNonNegative: Number => Boolean =
    value => value >= 0

  val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  @tailrec
  def addition(left: Number, right: Number): Number =
    if isZero(left) then right
    else if isNonNegative(left) then addition(decrement(left), increment(right))
    else addition(increment(left), decrement(right))

  def multiplication(left: Number, right: Number): Number =

    def negation(value: Number): Number =

      @tailrec
      def negationRec(value: Number, acc: Number): Number =
        if isZero(value) then acc
        else if isNonNegative(value) then negationRec(decrement(value), decrement(acc))
        else negationRec(increment(value), increment(acc))

      negationRec(value, acc = 0)

    @tailrec
    def multiplicationRec(left: Number, right: Number, acc: Number): Number =

      if isZero(right) then acc
      else if isNonNegative(right) then multiplicationRec(left, decrement(right), addition(acc, left))
      else multiplicationRec(left, increment(right), addition(acc, negation(left)))

    multiplicationRec(left, right, acc = 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerRec(base: Number, p: Number, acc: Number): Number =
      if isZero(p) then acc
      else powerRec(base, decrement(p), multiplication(acc, base))

    powerRec(base, p, acc = 1)
