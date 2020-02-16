package expression
import context._
import value._

case class Conjunction(operands: List[Expression]) extends SpecialForm{
  def execute(env: Environment) = {
    var result = true
    for(exp <- operands if result) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[Boole]) throw new TypeException("Conjunction operands must be Booles")
      result = arg.asInstanceOf[Boole].value
    }
    Boole(result)
  }
}