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

class TopValueCM(toTake: Int, minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
  
	def classify(doc: XMLDocument) : Set[String] = {
	  minions.map(m => ((m.getTopic, m.classify(doc))) ).toList.sortBy(-_._2).take(toTake).map(_._1).toSet
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