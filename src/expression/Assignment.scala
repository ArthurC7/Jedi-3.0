package expression
import context._
import value._

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    // 1. update vbl using update
    if(vbl.execute(env).isInstanceOf[Variable]){
      vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
      Notification.DONE
    }
    else throw new TypeException("Expected type: Variable")
  }
}