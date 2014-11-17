package dk.nezbo.ir.ass2

import scala.collection.immutable.Set
import ch.ethz.dal.classifier.processing.XMLDocument
import scala.util.Random

class LogisticRegression(topic: String, train: Seq[(Set[String],Array[Double])]) extends Classifier {
  
  // FIELDS
  
  val bias = 1.0 // TODO: What it should be?
  val random = new Random(1337)
  
  var theta: Array[Double] = (0 to train(0)._2.length).map(_ => 0.0/*random.nextDouble*/).toArray // TODO: What it should be?
  
  // INHERITED FUNCTIONS
  
  def getTopic() : String = {
    topic
  }
  
  def train(iteration: Integer) : Unit = {
    val lRate = 1.0/iteration.toDouble
    
    for(doc <- train){
    	val positive = doc._1.contains(topic)
    	theta = updateTheta(doc._2, theta, lRate, positive)	
    }
	    
  }
  def classify(doc: XMLDocument) : Boolean = {
    val prob = probRelevant(Main.lrFeatures(doc),theta)
    //println("Probability: "+prob)
    return prob > 0.5
  }
  
  override def toString = theta.toList.toString()
  
  // HELPER FUNCTIONS
  
  def innerProduct(v1: Array[Double], v2: Array[Double]) : Double = {
    v1.zip(v2).map(p => p._1 * p._2).sum
  }
  
  def probRelevant(dFeature: Array[Double], theta: Array[Double]) : Double = {
    val result = 1.0 / (1.0 + Math.exp(-1.0*bias - innerProduct(dFeature,theta)))
    //println(result)
    result
  }
  
  def scalarMultVector(scalar: Double, vector: Array[Double]) : Array[Double] = {
    vector.map(i => i*scalar)
  }
  
  def vectorAdd(v1: Array[Double], v2: Array[Double]) : Array[Double] = {
    v1.zip(v2).map(i => i._1 + i._2)
  }
  
  def deltaTheta(dFeature: Array[Double], theta: Array[Double], rel: Boolean) : Array[Double] = {
    if(rel){
      scalarMultVector(1.0 - probRelevant(dFeature,theta), dFeature)
    }else{
      scalarMultVector(-1.0*probRelevant(dFeature,theta), dFeature)
    }
  }
  
  def updateTheta(doc: Array[Double], theta: Array[Double], lRate: Double, rel: Boolean) : Array[Double] = {
    vectorAdd(scalarMultVector(lRate, deltaTheta(doc,theta,rel)), theta)
  }

}