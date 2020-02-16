package expression
import context._
import value._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    var result = false
    for(exp <- operands if !result) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[Boole]) throw new TypeException("Disjunction operands must be Booles")
      result = arg.asInstanceOf[Boole].value
    }
    Boole(result)
  }
}