package expression
import context._
import value._

case class Block(express: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    // 1. tempEnv extending env
    val tempEnv = new Environment(env)
    // 2. exp.execute(tempEnv) for each exp in block
    for(i <- 0 until express.size-1) express(i).execute(tempEnv)
    //for(exp <- express) exp.execute(env)
    // 3. return value of last one 
    express(express.size - 1).execute(tempEnv)
    
  }
}