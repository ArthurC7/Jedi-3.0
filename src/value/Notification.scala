package value

class Notification(val message: String) extends Value{
  override def toString = message
}

object Notification{
  def apply(message: String) = new Notification(message)
  
  def OK = Notification("OK")
  def DONE = Notification("DONE")
  def UNSPECIFIED = Notification("UNSPECIFIED")
}