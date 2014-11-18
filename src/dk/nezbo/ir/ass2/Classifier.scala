package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

trait Classifier {
	def getTopic() : String
  
	def train(iteration: Int) : Unit
	def classify(doc: XMLDocument) : Double
}