package kse.unit3.challenge

import kse.unit3.challenge.expressions.*
import kse.unit3.challenge.generators.given
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object BooleansSpecification extends Properties("Expressions"):

  include(BooleanEvaluationSpecification)
  include(VariableEvaluationSpecification)
  include(NegationEvaluationSpecification)
  include(ConjunctionEvaluationSpecification)
  include(DisjunctionEvaluationSpecification)
  include(ImplicationEvaluationSpecification)
  include(EquivalenceEvaluationSpecification)
  include(BooleanSubstitutionSpecification)
  include(VariableSubstitutionSpecification)
  include(ExpressionSubstitutionSpecification)

end BooleansSpecification

object BooleanEvaluationSpecification extends Properties("Boolean Evaluation"):

  property("boolean value should be evaluated to itself") = forAll: (boolean: Boolean) =>
    boolean.evaluate == boolean

end BooleanEvaluationSpecification

object VariableEvaluationSpecification extends Properties("Variable Evaluation"):

  property("variable should be evaluated to itself") = forAll: (variable: Variable) =>
    variable.evaluate == variable

end VariableEvaluationSpecification

object NegationEvaluationSpecification extends Properties("Negation Evaluation"):

  property("!True should be evaluated to False") = forAll: (boolean: Boolean) =>
    (!True).evaluate == False

  property("!False should be evaluated to True") = forAll: (boolean: Boolean) =>
    (!False).evaluate == True

  property("!variable should be evaluated to !variable") = forAll: (variable: Variable) =>
    (!variable).evaluate == !variable

  property("!expression should be correctly evaluated") = forAll: (expression: Expression) =>
    (!expression).evaluate == (!expression.evaluate).evaluate

end NegationEvaluationSpecification

object ConjunctionEvaluationSpecification extends Properties("Conjunction Evaluation"):

  property("True ∧ expression should be evaluated to expression evaluation") = forAll: (expression: Expression) =>
    (True ∧ expression).evaluate == expression.evaluate

  property("expression ∧ True should be evaluated to expression evaluation") = forAll: (expression: Expression) =>
    (expression ∧ True).evaluate == expression.evaluate

  property("False ∧ expression should be evaluated to False") = forAll: (expression: Expression) =>
    (False ∧ expression).evaluate == False

  property("expression ∧ False should be evaluated to False") = forAll: (expression: Expression) =>
    (expression ∧ False).evaluate == False

  property("left ∧ right should be correctly evaluated") = forAll: (left: Boolean, right: Boolean) =>
    (left ∧ right).evaluate == (left.evaluate ∧ right.evaluate).evaluate

end ConjunctionEvaluationSpecification

object DisjunctionEvaluationSpecification extends Properties("Disjunction Evaluation"):

  property("True ∨ expression should be evaluated to True") = forAll: (expression: Expression) =>
    (True ∨ expression).evaluate == True

  property("expression ∨ True should be evaluated to True") = forAll: (expression: Expression) =>
    (expression ∨ True).evaluate == True

  property("False ∨ expression should be evaluated to expression evaluation") = forAll: (expression: Expression) =>
    (False ∨ expression).evaluate == expression.evaluate

  property("expression ∨ False should be evaluated to expression evaluation") = forAll: (expression: Expression) =>
    (expression ∨ False).evaluate == expression.evaluate

  property("left ∨ left should be correctly evaluated") =
    forAll((left: Boolean, right: Boolean) => (left ∨ right).evaluate == (left.evaluate ∨ right.evaluate).evaluate)

end DisjunctionEvaluationSpecification

object ImplicationEvaluationSpecification extends Properties("Implication Evaluation"):

  property("True → expression should be evaluated to expression evaluation") = forAll: (expression: Expression) =>
    (True → expression).evaluate == expression.evaluate

  property("False → expression should be evaluated to True") = forAll: (expression: Expression) =>
    (False → expression).evaluate == True

  property("left → right should be correctly evaluated") = forAll: (left: Expression, right : Expression) =>
    (left.evaluate → right.evaluate).evaluate == (left → right).evaluate

end ImplicationEvaluationSpecification

object EquivalenceEvaluationSpecification extends Properties("Equivalence Evaluation"):

  property("Reflexivity") = forAll: (a: Boolean) =>
    (a ↔ a).evaluate == True

  property("Symmetry") = forAll: (a: Boolean, b: Boolean) =>
    Equivalence(a, b).evaluate == Equivalence(b, a).evaluate

  property("Transitivity") = forAll: (a: Boolean, b: Boolean, c: Boolean) =>
    ((a ↔ b).evaluate == True && (b ↔ c).evaluate == True) ==> {
      (a ↔ c).evaluate == True
    }

  property("left ↔ left should be correctly evaluated") = forAll: (left: Boolean) =>
    (left ↔ left).evaluate == True

end EquivalenceEvaluationSpecification

object BooleanSubstitutionSpecification extends Properties("Boolean Substitution"):

  property("substitution into boolean should make no changes") = forAll: (b: Boolean, substitution: Expression) =>
    b.substitute(Variable("x"), substitution) == b

end BooleanSubstitutionSpecification

object VariableSubstitutionSpecification extends Properties("Variable Substitution"):

  property("substitution into different variable should make no changes") = forAll: (v1: Variable, v2: Variable, substitution: Expression) =>
    v1 != v2 ==> {
      v1.substitute(v2, substitution) == v1
    }

  property("substitution into the same variable should return the given expression") = forAll: (v: Variable, substitution: Expression) =>
    v.substitute(v, substitution) == substitution

end VariableSubstitutionSpecification

object ExpressionSubstitutionSpecification extends Properties("Expression Substitution"):

  property("substitution into !expression should be equal to !(substitution into expression)") = forAll:
    (expression: Expression, variable: Variable, substitution: Expression) =>
      (!expression).substitute(variable, substitution) == (!(expression.substitute(variable, substitution)))

  property("substitution into left ∧ right should be equal to substitution into left ∧ substitution into right") = forAll:
    (left: Expression, right: Expression, variable: Variable, substitution: Expression) =>
      (left ∧ right).substitute(variable, substitution) == (left.substitute(variable, substitution) ∧ right.substitute(variable, substitution))

  property("substitution into left ∨ right should be equal to substitution into left ∨ substitution into right") = forAll:
    (left: Expression, right: Expression, variable: Variable, substitution: Expression) =>
      (left ∨ right).substitute(variable, substitution) == (left.substitute(variable, substitution) ∨ right.substitute(variable, substitution))

  property("substitution into left → right should be equal to substitution into left → substitution into right") = forAll:
    (left: Expression, right: Expression, variable: Variable, substitution: Expression) =>
      (left → right).substitute(variable, substitution) == (left.substitute(variable, substitution) → right.substitute(variable, substitution))

  property("substitution into left ↔ right should be equal to substitution into left ↔ substitution into right") = forAll:
    (left: Expression, right: Expression, variable: Variable, substitution: Expression) =>
      (left ↔ right).substitute(variable, substitution) == (left.substitute(variable, substitution) ↔ right.substitute(variable, substitution))

end ExpressionSubstitutionSpecification
