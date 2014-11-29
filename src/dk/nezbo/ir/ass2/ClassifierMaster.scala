package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.XMLDocument

/**
 * The class is responsible for handling the numerous classifiers
 * given to it at creation time. It will train them all in parallel
 * when instructed to and aggregate the results of classifications
 * with its individual logic for selection.
 */
abstract class ClassifierMaster(minions: Iterable[Classifier]) {

  /**
   * Trains all the individual classifiers in parallel for the
   * requested number of iterations and then wipes their training
   * data for efficient memory usage (i.e. expecting the training
   * to be done).
   */
  def train(numIterations: Int) : Unit = {
	var operation = 0
	 val total = (minions.size * numIterations).toDouble
	  
	for(m <- minions.par){
	  for(i <- (1 to numIterations)){
		 m.train(i)
		 operation = operation + 1 // lets cross fingers for no collisions (just for print anyway)
		 if(i % 10000 == 0) println("\t"+((operation.toDouble/total)*100).toInt+"% ["+m.getTopic+"] Iteration "+i+" done.")
	  }
	  m.clearTrain
	}
  }
  
  /**
   * Gets the number of classifiers contained inside.
   */
  def size = minions.size
	
  /**
   * The abstract method to classify the given document with
   * all minions and return a subset of the topics as the
   * result.
   */
  def classify(doc: XMLDocument) : Set[String]
}

/**
 * This master returns the topics of the Top X classifiers by their
 * output value.
 */
class TopValueCM(toTake: Int, minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
  
  def classify(doc: XMLDocument) : Set[String] = {
	minions.map(m => ((m.getTopic, m.classify(doc))) ).toList.sortBy(-_._2).take(toTake).map(_._1).toSet
  }
}

/**
 * This master returns the topics of all classifiers with a resulting
 * value of more than the given constant.
 */
class OverConstCM(value: Double, minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
  def classify(doc: XMLDocument) : Set[String] = {
	minions.filter(m => m.classify(doc) > value).map(_.getTopic).toSet
  }
}

/**
 * This master (not used in final result) gives the (twice)
 * above average topics. So, those classifiers that are above
 * the average value of all the classifiers that are above the
 * average :).
 */
class OverAverage2CM(minions: Iterable[Classifier]) extends ClassifierMaster(minions: Iterable[Classifier]) {
  def classify(doc: XMLDocument) : Set[String] = {
	val values = minions.map(m => ((m.getTopic, m.classify(doc))) )
	overAverage(overAverage(values)).map(_._1).toSet
  }
	
  def overAverage(values: Iterable[(String,Double)]) : Iterable[(String,Double)] = {
	val average = values.map(_._2).sum.toDouble / minions.size.toDouble
	values.filter(_._2 > average)
  }
}