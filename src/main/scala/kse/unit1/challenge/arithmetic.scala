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

    @tailrec
    def multiplicationRec(left: Number, right: Number, acc: Number): Number =

      if isZero(right) || isZero(left) then acc
      else if isNonNegative(right) && isNonNegative(left) then multiplicationRec(left, decrement(right), addition(acc, left))
      else if !isNonNegative(right) && !isNonNegative(left) then multiplicationRec(abs(left), decrement(abs(right)), addition(acc, abs(left)))
      else if !isNonNegative(left) && isNonNegative(right) then multiplicationRec(left, decrement(right), addition(acc, left))
      else multiplicationRec(decrement(left), right, addition(acc, right))

    multiplicationRec(left, right, acc = 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerRec(base: Number, p: Number, acc: Number): Number =
      if isZero(p) then acc
      else powerRec(base, decrement(p), multiplication(acc, base))

    powerRec(base, p, acc = 1)
