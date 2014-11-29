package dk.nezbo.ir.ass2

import com.github.aztek.porterstemmer.PorterStemmer
import scala.collection.mutable.HashMap
import ch.ethz.dal.classifier.processing.XMLDocument
import scala.collection.mutable.PriorityQueue

/**
 * Various utility methods that are helpful in other
 * parts of the program.
 */
object Utilities {
  
  // FIELDS
  
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
  
  // METHODS
  
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
  
  /**
   * Retrieves the term frequencies of a document while
   * also removing stopwords and very short tokens and
   * reduces words to their stem.
   */
  def getTermFrequencies(doc: XMLDocument) : Map[String,Int] = {
    doc.tokens.filter(t => !stopWords.contains(t) && t.length() > 2).map(Utilities.getStem(_)).groupBy(identity).mapValues(v => v.length)
  }
  
  /**
   * Calculates the F1 Score using the given precision
   * and recall values.
   */
  def f1score(prec: Double, recall: Double) : Double = {
    if(prec+recall < 0.0001){
      return 0.0
    }
    2.0 * (prec*recall) / (prec+recall)
  }
  
  /**
   * Calculates the precision of a classification using
   * the Set of found elements compared to the Set of
   * relevant elements. The elements can be of any type.
   */
  def precision[T](found: Set[T], relevant: Set[T]) : Double = {
    if(found.size == 0){
      return 0.0
    }
    found.intersect(relevant).size.toDouble / found.size.toDouble
  }
  
  /**
   * Calculates the recall of a classification using
   * the Set of found elements compared to the Set of
   * relevant elements. The elements can be of any type.
   */
  def recall[T](found: Set[T], relevant: Set[T]) : Double = {
    if(relevant.size == 0){
      return 0.0
    }
    found.intersect(relevant).size.toDouble / relevant.size.toDouble
  }
  
  /**
   * Calculates the tf-idf value using the term frequency,
   * document frequency and total number of documents.
   */
  def tfidf(tf: Int, df: Int, docs: Int) = {
    (1+Math.log10(tf)) * idf(df,docs)
  }
  
  // PRIVATE HELPER METHODS
  
  private def idf(df: Int, n: Int) = {
    Math.log(n) - Math.log(df)
  }
}