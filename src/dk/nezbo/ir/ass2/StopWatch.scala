package dk.nezbo.ir.ass2

/**
 * A little (static) helper object to track time spent.
 */
object StopWatch {
	
  private var t0 = System.currentTimeMillis() // start time
  private var last = t0 // last tick
  
  /**
   * Starts the StopWatch again.
   */
  def start = t0 = System.currentTimeMillis()
  
  /**
   * Prints the current time elapsed since last tick and in total.
   */
  def tick = println("Time elapsed: "+timeSinceLast+" (total: "+toString+")")
  
  /**
   * Stop just prints the total time.
   */
  def stop = println("Total time: "+toString)
  
  /**
   * Converts the difference between now and last tick to a string.
   */
  private def timeSinceLast() : String = {
    val cur = System.currentTimeMillis()
    val result = formatTime(cur - last)
    last = cur
    result
  }
  
  /**
   * The string representation of a time interval.
   */
  private def formatTime(time : Long) : String = {
    val sb = new StringBuilder()
    val sec = ((time / 1000.0) % 60.0).toInt
    val min = (time / 60000.0).toInt
    if(min > 0) sb.append(min+" min ")
    sb.append(sec+" sec")
    sb.toString
  }
  
  override def toString = {
    formatTime(System.currentTimeMillis() - t0)
  }
}