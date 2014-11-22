package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

class NaiveBayes(topic: String, totTFS: Map[String,Int], totLength: Int, totCDocs: Int, totDocs: Int, vocSize: Int) extends Classifier {
	
	def alpha = 1.0
	def logpHatC = Math.log10(totCDocs.toDouble / totDocs.toDouble)
  
	def getTopic() : String = {
	  topic
	}
  
	def train(iteration: Int) : Unit = {}
	
	def classify(doc: XMLDocument) : Double = {
	  val _2 = Utilities.getTermFrequencies(doc).map(kv => kv._2 * logpHatWC(kv._1)).sum
	  logpHatC + _2
	}
	
	// HELPERS
	
	private def logpHatWC(word: String) : Double = {
	  val top = totTFS.getOrElse(word, 0).toDouble + alpha
	  val bottom = totLength + alpha * vocSize
	  
	  top / bottom
	}
}