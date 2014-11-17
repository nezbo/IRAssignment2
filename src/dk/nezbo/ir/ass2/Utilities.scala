package dk.nezbo.ir.ass2

import com.github.aztek.porterstemmer.PorterStemmer
import scala.collection.mutable.HashMap
import ch.ethz.dal.classifier.processing.XMLDocument

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
  
  def getTermFrequencies(doc: XMLDocument) : Map[String,Int] = {
    doc.tokens.map(Utilities.getStem(_)).groupBy(identity).mapValues(v => v.length)
  }
}