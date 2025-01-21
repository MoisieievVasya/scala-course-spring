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

  def addition(left: Number, right: Number): Number =

    @tailrec
    def additionRec(left: Number, right: Number): Number =
      if !isNonNegative(left) then
        additionRec(decrement(left), increment(right))

      else if isZero(right) then
        left

      else
        additionRec(increment(left), decrement(right))

    additionRec(left, right)

  def multiplication(left: Number, right: Number): Number =
    require(left >= 0, "Left must be non-negative")
    require(right >= 0, "Right must be non-negative")

    @tailrec
    def multiplicationRec(left: Number, right: Number, acc: Number): Number =
      if !isNonNegative(left) then
        multiplicationRec(left, decrement(right), addition(acc, left))

      else if isZero(right) then
        acc

      else
        multiplicationRec(left, increment(right), addition(acc, -left))

    multiplicationRec(left, right, acc = 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerRec(base: Number, p: Number, acc: Number): Number =
      if isZero(p) then acc
      else powerRec(base, decrement(p), multiplication(acc, base))

    powerRec(base, p, acc = 1)
