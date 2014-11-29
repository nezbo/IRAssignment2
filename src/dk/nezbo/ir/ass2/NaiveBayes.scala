package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

/**
 * A classifier that uses the Naive Bayes approach, but receives
 * the pre-calculated values for efficient processing:
 * The summed term frequencies for all documents containing this topic.
 * The total length (number of tokens) in documents of this topic.
 * The total number of documents with this topic.
 * The overall total number of training document.
 * The vocabulary size of the training collection.
 */
class NaiveBayes(topic: String, totTFS: Map[String,Int], totLength: Int, totCDocs: Int, totDocs: Int, vocSize: Int) extends Classifier(topic:String) {
	
	def alpha = 1.0
	def logpHatC = Math.log10(totCDocs.toDouble / totDocs.toDouble) // since this term is a constant we can pre-calculate it
  
	// no need for training
	def train(iteration: Int) : Unit = {}
	def clearTrain = {}
	
	def classify(doc: XMLDocument) : Double = {
	  val _2 = Utilities.getTermFrequencies(doc).map(kv => kv._2 * logpHatWC(kv._1)).sum
	  logpHatC + _2
	}
	
	// HELPERS
	
	/**
	 * Calculates the log of the probability of a word given the class.
	 */
	private def logpHatWC(word: String) : Double = {
	  val top = totTFS.getOrElse(word, 0).toDouble + alpha
	  val bottom = totLength + alpha * vocSize
	  
	  top / bottom
	}
}