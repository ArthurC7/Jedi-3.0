package expression
import value._
import context._

case class Declaration(val identifier: Identifier, val expression: Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    // 1. initVal = execute expression
    //val initVal = 
    // 2. update env
    env.put(identifier, expression.execute(env))
    Notification.DONE
  }
}