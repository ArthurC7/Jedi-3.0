package expression
import value._
import context._

case class Lambda(body: Expression, parameters: List[Identifier]) extends SpecialForm{
  def execute(env: Environment): Value = {
    // Returns a Closure
    Closure(body, parameters, env)
  }
}