package value
import context._
import expression._

case class Closure(body: Expression, parameters: List[Identifier], defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    if(parameters.size != args.size) throw new TypeException("parameters do not match arguments")
    // 1. tempEnv extends defEnv
    val tempEnv = new Environment(defEnv)
    // 2. tempEnv.BulkPut(); may be buggy
    tempEnv.bulkPut(parameters, args)
    // 3. body.execute(tempEnv)
    body.execute(tempEnv)
  }
}