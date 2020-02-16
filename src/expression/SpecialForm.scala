package expression
import value.Value
import context.Environment

trait SpecialForm extends Expression{
  def execute(env: Environment): Value
}