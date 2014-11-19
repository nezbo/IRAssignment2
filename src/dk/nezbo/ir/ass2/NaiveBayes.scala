package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

class NaiveBayes(topic: String, totTFS: Map[String,Int], totLength: Int, totCDocs: Int, totDocs: Int) extends Classifier {
	
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
	  val result = totTFS.getOrElse(word, 0).toDouble / totLength
	  
	  if(result > 0.0){
	    return Math.log10(result)
	  }
	  0.0
	}
}