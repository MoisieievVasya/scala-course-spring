package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  include(ZeroSpecification)
  include(SuccessorSpecification)
  include(NumeralSpecification)

end NumeralsSpecification

object ZeroSpecification extends Properties("Zero"):

  property("isZero") = forAll: (z: Zero) =>
    z.isZero

  property("predecessor") = forAll: (z: Zero) =>
    z.predecessor == z

  property("successor") = forAll: (z: Zero) =>
    z.successor == Successor(z)

  property("greater than") = forAll: (z: Zero, s: Successor) =>
    !(z > s)

  property("greater or equal to") = forAll: (z: Zero, s: Successor) =>
    !(z >= s)

  property("less than") = forAll: (z: Zero, s: Successor) =>
    z < s

  property("less or equal to") = forAll: (z: Zero, s: Successor) =>
    z <= s

  property("addition") = forAll: (z: Zero, s: Successor) =>
    z + s == s

  property("toInt") = forAll: (z: Zero) =>
    z.toInt == 0

  property("toString") = forAll: (z: Zero) =>
    z.toString == "0"

  property("equals") = forAll: (z: Zero) =>
    z == Zero

  property("subtraction Zero - Successor") = forAll: (z: Zero, s: Successor) =>
    z - s == z

  property("subtraction Successor - Zero") = forAll: (s: Successor, z: Zero) =>
    s - z == s

end ZeroSpecification

object SuccessorSpecification extends Properties("Successor"):

  property("isZero") = forAll: (s: Successor) =>
    !s.isZero

  property("predecessor") = forAll: (s: Successor) =>
    s.predecessor == s.predecessor

  property("successor") = forAll: (s: Successor) =>
    s.successor == Successor(s)

  property("greater than") = forAll: (s1: Successor, s2: Successor) =>
    s1 > s2 == s1.predecessor > s2.predecessor

  property("greater or equal to") = forAll: (s1: Successor, s2: Successor) =>
    s1 >= s2 == s1.predecessor >= s2.predecessor

  property("less than") = forAll: (s1: Successor, s2: Successor) =>
    s1 < s2 == s1.predecessor < s2.predecessor

  property("less or equal to") = forAll: (s1: Successor, s2: Successor) =>
    s1 <= s2 == s1.predecessor <= s2.predecessor

  property("addition") = forAll: (s1: Successor, s2: Successor) =>
    s1 + s2 == Successor(s1 + s2.predecessor)

  property("toInt") = forAll: (s: Successor) =>
    s.toInt == s.predecessor.toInt + 1

  property("toString") = forAll: (s: Successor) =>
    s.toString == "Nat(" + s.predecessor + ")"

  property("equals") = forAll: (s: Successor) =>
    s == Successor(s.predecessor)

  property("subtraction Successor - Zero") = forAll: (s: Successor, z: Zero) =>
    s - z == s

  property("subtraction Successor - Successor") = forAll: (s1: Successor, s2: Successor) =>
    s1 - s2 == s1.successor - s2.successor

end SuccessorSpecification

object NumeralSpecification extends Properties("Numeral"):

  property("greater than") = forAll: (n1: Numeral) =>
    n1 > n1.predecessor

  property("greater or equal to") = forAll: (n1: Numeral, n2: Numeral) =>
    n1 >= n2 == n1.toInt >= n2.toInt

  property("less than") = forAll: (n1: Numeral, n2: Numeral) =>
    n1 < n2 == n1.toInt < n2.toInt

  property("less or equal to") = forAll: (n1: Numeral, n2: Numeral) =>
    n1 <= n2 == n1.toInt <= n2.toInt

  property("ToString") = forAll: (n: Numeral) =>
    if n.isZero then n.toString == "0"
    else n.toString == "Nat(" + n.predecessor + ")"

end NumeralSpecification
