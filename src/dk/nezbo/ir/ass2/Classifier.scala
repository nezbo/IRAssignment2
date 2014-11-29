package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

/**
 * The abstract class defining the methods that a
 * classifier for the system must implement. The
 * training documents are expected to be input to
 * the class itself in the format it wants.
 */
abstract class Classifier(topic: String) {
	def getTopic() : String = topic
  
	def train(iteration: Int) : Unit
	def clearTrain : Unit
	def classify(doc: XMLDocument) : Double
}