package expression
import context._
import value._

// if (condition) consequent (else alternative) 
case class Conditional(val condition: Expression, consequent: Expression, val alternative: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    val val1 = condition.execute(env)
    if(!val1.isInstanceOf[Boole]) throw new TypeException("conditions must be Boole-valued")
    val val2 = val1.asInstanceOf[Boole]
    val val3 = val2.value
    
    if(val3) consequent.execute(env)
    else if (alternative == null) Notification.UNSPECIFIED
    else alternative.execute(env)
  }
}