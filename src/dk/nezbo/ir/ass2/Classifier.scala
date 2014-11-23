package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

abstract class Classifier(topic: String) {
	def getTopic() : String = topic
  
	def train(iteration: Int) : Unit
	def clearTrain() : Unit
	def classify(doc: XMLDocument) : Double
}