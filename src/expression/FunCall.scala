package expression
import context._
import value._

case class FunCall(val operator: Identifier, operands: List[Expression]) extends Expression{
  def execute(env: Environment) = {
    // 1. args = result of executing operands
    val args = operands.map((oper: Expression) => oper.execute(env))

    // 2. if operator is already in env, you execute it, assign it to a closure
    if(env.contains(operator)){
      val closure = operator.execute(env)
      // 3. apply the closure
      closure.asInstanceOf[Closure].apply(args)
    }
    // 4. else execute it normally in the alu 
    else alu.execute(operator, args)
  }
}