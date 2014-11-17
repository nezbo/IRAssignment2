package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

class NaiveBayes(topic: String, cDocs: Seq[(Map[String,Int],Int)], totDocs: Int) extends Classifier {
  
	def totLength = cDocs.map(d => d._2).sum.toDouble
	def logpHatWCmap = cDocs.flatMap(d => d._1.toIterable)
			.groupBy(t => t._1)
			.map(kv => (kv._1 -> kv._2.map(t => t._2).sum.toDouble / totLength))
	
	def logpHatC = Math.log10(cDocs.length.toDouble / totDocs.toDouble)
  
	def getTopic() : String = {
	  topic
	}
  
	def train(iteration: Integer) : Unit = {}
	
	def classify(doc: XMLDocument) : Boolean = {
	  val _2 = Utilities.getTermFrequencies(doc).map(kv => kv._2 * logpHatWC(kv._1)).sum
	  val value = logpHatC + _2
	  println(logpHatC +"+"+_2+"="+value)
	  
	  (value > 0.5)
	}
	
	private def logpHatWC(word: String) : Double = {
	  val result = logpHatWCmap.getOrElse(word, 0.0)
	  
	  if(result > 0.0){
	    return Math.log10(result)
	  }
	  0.0
	}
}