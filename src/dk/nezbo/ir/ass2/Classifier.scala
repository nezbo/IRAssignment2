package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

trait Classifier {
	def train(iteration: Integer) : Unit
	def classify(doc: XMLDocument) : Boolean
}