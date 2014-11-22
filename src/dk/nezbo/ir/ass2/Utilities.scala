package dk.nezbo.ir.ass2

import com.github.aztek.porterstemmer.PorterStemmer
import scala.collection.mutable.HashMap
import ch.ethz.dal.classifier.processing.XMLDocument
import scala.collection.mutable.PriorityQueue

object Utilities {
  
  val stopWords = Set("a","about","above","after","again","against","all","am","an","and","any","are","aren't","as","at",
      "be","because","been","before","being","below","between","both","but","by","can't","cannot","could","couldn't",
      "did","didn't","do","does","doesn't","doing","don't","down","during","each","few","for","from","further",
      "had","hadn't","has","hasn't","have","haven't","having","he","he'd","he'll","he's","her","here","here's","hers",
      "herself","him","himself","his","how","how's","i","i'd","i'll","i'm","i've","if","in","into","is","isn't","it",
      "it's","its","itself","let's","me","more","most","mustn't","my","myself","no","nor","not","of","off","on","once",
      "only","or","other","ought","our","ours","ourselves","out","over","own","same","shan't","she","she'd","she'll",
      "she's","should","shouldn't","so","some","such","than","that","that's","the","their","theirs","them","themselves",
      "then","there","there's","these","they","they'd","they'll","they're","they've","this","those","through","to","too",
      "under","until","up","very","was","wasn't","we","we'd","we'll","we're","we've","were","weren't","what","what's","when",
      "when's","where","where's","which","while","who","who's","whom","why","why's","with","won't","would","wouldn't",
      "you","you'd","you'll","you're","you've","your","yours","yourself","yourselves")

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
    doc.tokens.filter(!stopWords.contains(_)).map(Utilities.getStem(_)).groupBy(identity).mapValues(v => v.length)
  }
  
  // EXTENSION METHODS
  
  	class InclusiveIterator[T](ia: Seq[T]) {
    def top(amount: Int, lambda: (T => Double)) : Iterable[T] = {
      val priority = new PriorityQueue[T]()(Ordering.by(x => -1.0-lambda(x)))
      for(item <- ia){
        priority += item
        priority.dequeue
      }
      priority.toList.sortBy(lambda)
    }
	}
  	
	implicit def iterator_can_include[A](ia: Seq[A]) = new InclusiveIterator(ia)
  
  def tfidf(tf: Int, df: Int, docs: Int) = {
    (1+Math.log10(tf)) * idf(df,docs)
  }
  
  private def idf(df: Int, n: Int) = {
    Math.log(n) - Math.log(df)
  }
}