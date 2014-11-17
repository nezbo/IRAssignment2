package dk.nezbo.ir.ass2

import com.github.aztek.porterstemmer.PorterStemmer
import scala.collection.mutable.HashMap

object Utilities {

    // cache to reduce time stemming common words, cleared often to limit memory usage
  val stemCache : HashMap[String,String] = new HashMap[String,String]
  
  /**
   * Retrieves the stem of the word if it has already been
   * calculated, or calculated from scratch and cached.
   */
  def getStem(word : String) : String = {
    if(stemCache.size > 500000) stemCache.clear
    
    if(!stemCache.contains(word)){
      stemCache.put(word, PorterStemmer.stem(word))
    }
    stemCache(word)
  }
}