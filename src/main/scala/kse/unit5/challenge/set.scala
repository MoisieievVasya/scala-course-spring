package kse.unit5.challenge

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object set:
  
  trait NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean

    infix def exists(predicate: Numeral => Boolean): Boolean

    infix def contains(x: Numeral): Boolean

    infix def include(x: Numeral): NumeralSet

    // Optional
    // Uncomment if needed
    infix def remove(x: Numeral): NumeralSet

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet

    // Optional
    // Uncomment if needed
    @targetName("difference")
    infix def \(that: NumeralSet): NumeralSet

    // Optional
    // Uncomment if needed
    @targetName("symmetric difference")
    infix def ∆(that: NumeralSet): NumeralSet = (this ∪ that) \ (this ∩ that)

  end NumeralSet

  type Empty = Empty.type

  case object Empty extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean = true

    infix def exists(predicate: Numeral => Boolean): Boolean = false

    infix def contains(x: Numeral): Boolean = false

    infix def include(x: Numeral): NumeralSet = NonEmpty(Empty, x, Empty)

    // Optional
    // Uncomment if needed
    infix def remove(x: Numeral): NumeralSet = Empty

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet = that

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet = Empty

    // Optional
    // Uncomment if needed
    @targetName("difference")
    infix def \(that: NumeralSet): NumeralSet = Empty

    override def toString: String = "[*]"

    override def equals(obj: Any): Boolean =
      obj match
        case _: Empty.type => true
        case _             => false

    override def hashCode(): Int = 0

  end Empty

  case class NonEmpty(left: NumeralSet, element: Numeral, right: NumeralSet) extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean =
      left.forAll(predicate) && predicate(element) && right.forAll(predicate)

    infix def exists(predicate: Numeral => Boolean): Boolean =
      left.exists(predicate) || predicate(element) || right.exists(predicate)

    infix def contains(x: Numeral): Boolean =
      if x < element then left.contains(x)
      else if x > element then right.contains(x)
      else x == element

    infix def include(x: Numeral): NumeralSet =
      if x < element then NonEmpty(left.include(x), element, right)
      else if x > element then NonEmpty(left, element, right.include(x))
      else this

    // Optional
    // Uncomment if needed
    infix def remove(x: Numeral): NumeralSet =
      if x < element then NonEmpty(left.remove(x), element, right)
      else if x > element then NonEmpty(left, element, right.remove(x))
      else left ∪ right

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet =
      left ∪ right ∪ that.include(element)

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet =
      if that.contains(element) then NonEmpty(left ∩ that, element, right ∩ that)
      else (left ∩ that) ∪ (right ∩ that)

    // Optional
    // Uncomment if needed
    @targetName("difference")
    infix def \(that: NumeralSet): NumeralSet =
      if that.contains(element) then (left \ that) ∪ (right \ that)
      else NonEmpty(left \ that, element, right \ that)

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean =
      obj match
        case that: NumeralSet =>
          this.forAll(that.contains) && that.forAll(this.contains)
        case _ => false

    override def hashCode(): Int =
      left.hashCode() + element.hashCode() + right.hashCode()

  end NonEmpty

end set
