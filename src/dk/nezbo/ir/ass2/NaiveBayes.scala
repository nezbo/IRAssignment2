package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

class NaiveBayes(topic: String) extends Classifier {
  
	def getTopic() : String = {
	  topic
	}
  
	def train(iteration: Integer) : Unit = {
	  
	}
	def classify(doc: XMLDocument) : Boolean = {
	  false
	}
}