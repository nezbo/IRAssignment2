package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

abstract class ClassifierMaster(minions: Iterable[Classifier]) {

	def train(numIterations: Int) : Unit = {
	  var operation = 0
	  val total = (minions.size * numIterations).toDouble
	  
	  for(m <- minions.par){
	    for(i <- (1 to numIterations)){
		    m.train(i)
		    operation = operation + 1 // lets cross fingers for no collisions
		    if(i % 1000 == 0) println("\t"+((operation.toDouble/total)*100).toInt+"% ["+m.getTopic+"] Iteration "+i+" done.")
	    }
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

class OverConstCM(value: Double, minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
	def classify(doc: XMLDocument) : Set[String] = {
	  minions.filter(m => m.classify(doc) > value).map(_.getTopic).toSet
	}
}

class OverAverageCM(minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
	def classify(doc: XMLDocument) : Set[String] = {
	  val values = minions.map(m => ((m.getTopic, m.classify(doc))) )
	  overAverage(overAverage(values)).map(_._1).toSet
	}
	
	def overAverage(values: Iterable[(String,Double)]) : Iterable[(String,Double)] = {
	  val average = values.map(_._2).sum.toDouble / minions.size.toDouble
	  values.filter(_._2 > average)
	}
}