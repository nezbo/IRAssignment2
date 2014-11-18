package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

abstract class ClassifierMaster(minions: Iterable[Classifier]) {
	def train(iteration: Int) : Unit = {
	  for(m <- minions.par){
	    m.train(iteration)
	    println("\t["+m.getTopic+"] Iteration "+iteration+" done.")
	  }
	}
	def size = minions.size
	
	def classify(doc: XMLDocument) : Set[String]
}

class TopValueCM(minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
  
	def classify(doc: XMLDocument) : Set[String] = {
	  Set(minions.map(m => ((m.getTopic, m.classify(doc))) ).maxBy(_._2)._1)
	}
}

class OverPointFiveCM(minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
	def classify(doc: XMLDocument) : Set[String] = {
	  minions.filter(m => m.classify(doc) > 0.5).map(_.getTopic).toSet
	}
}

class OverAverageCM(minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
	def classify(doc: XMLDocument) : Set[String] = {
	  val values = minions.map(m => ((m.getTopic, m.classify(doc))) )
	  val average = values.map(_._2).sum.toDouble / minions.size.toDouble
	  values.filter(_._2 > average).map(_._1).toSet
	}
}