package kse.unit2.challenge

import scala.annotation.{tailrec, targetName}

object booleans:

  case object True
  case object False

  type True    = True.type
  type False   = False.type
  type Boolean = True | False

  val negation: Boolean => Boolean = { case True => False; case False => True }

  val conjunction: (Boolean, => Boolean) => Boolean = (left, right) =>
    if left == False then False else (left, right) match { case (True, value) => value; case (False, _) => False }

  val disjunction: (Boolean, => Boolean) => Boolean = (left, right) =>
    if left == True then True else (left, right) match { case (True, _) => True; case (False, value) => value }

  val implication: (Boolean, => Boolean) => Boolean = (left, right) =>
    if left == False then True else (left, right) match { case (True, value) => value; case (False, _) => True }

  val equivalence: (Boolean, => Boolean) => Boolean = { case (True, value) => value; case (False, value) => negation(value) }

  extension (value: Boolean)

    @targetName("negation")
    infix def unary_! : Boolean = negation(value)

    @targetName("conjunction")
    infix def ∧(that: => Boolean): Boolean = conjunction(value, that)

    @targetName("disjunction")
    infix def ∨(that: => Boolean): Boolean = disjunction(value, that)

    @targetName("implication")
    infix def →(that: => Boolean): Boolean = implication(value, that)

    @targetName("equivalence")
    infix def ↔(that: => Boolean): Boolean = equivalence(value, that)

  def fold(operation: (Boolean, Boolean) => Boolean, unit: Boolean)(list: List[Boolean]): Boolean =

    @tailrec
    def foldRec(list: List[Boolean], acc: Boolean): Boolean = list match
      case Nil          => acc
      case head :: tail => foldRec(tail, operation(head, acc))

    foldRec(list, unit)

  val conjunctionOfElements: List[Boolean] => Boolean = fold((left, right) => left ∧ right, True)
  val disjunctionOfElements: List[Boolean] => Boolean = fold((left, right) => left ∨ right, False)

  extension (booleans: List[Boolean])
    infix def conjunction: Boolean = conjunctionOfElements(booleans)
    infix def disjunction: Boolean = disjunctionOfElements(booleans)
