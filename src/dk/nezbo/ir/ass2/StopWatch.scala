package dk.nezbo.ir.ass2

class StopWatch {
	
  private val t0 = System.currentTimeMillis()
  private var last = System.currentTimeMillis()
  
  def tick = println("Time elapsed: "+timeSinceLast)
  
  private def timeSinceLast() : String = {
    val cur = System.currentTimeMillis()
    val result = formatTime(cur - last)
    last = cur
    result
  }
  
  private def formatTime(time : Long) : String = {
    val sb = new StringBuilder()
    val sec = ((time / 1000.0) % 60.0).toInt
    val min = (time / 60000.0).toInt
    if(min > 0) sb.append(min+" min ")
    if(sec > 0) sb.append(sec+" s")
    sb.toString
  }
  
  override def toString = {
    "Time elapsed: "+formatTime(System.currentTimeMillis() - t0)
  }
}