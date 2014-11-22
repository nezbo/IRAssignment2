package dk.nezbo.ir.ass2

import scala.collection.immutable.Set
import ch.ethz.dal.classifier.processing.XMLDocument
import scala.util.Random
import scala.collection.mutable.HashMap

class LogisticRegression(topic: String, trainSet: Seq[(Set[String],Map[String,Double])]) extends Classifier {
  
  // FIELDS
  
  val bias = 1.0 // TODO: What it should be?
  
  var theta: HashMap[String,Double] = HashMap[String,Double]()
  
  // INHERITED FUNCTIONS
  
  def getTopic() : String = {
    topic
  }
  
  def train(iteration: Int) : Unit = {    
    for(doc <- trainSet){
    	//println("["+topic+"] theta size: "+theta.size)
    	val positive = doc._1.contains(topic)
    	updateTheta(doc._2, theta, iteration, positive)	
    }
    //println("["+topic+"] theta size: "+theta.size)
	    
  }
  def classify(doc: XMLDocument) : Double = {
    val prob = probRelevant(Main.lrFeatures(doc),theta)
    //println("Probability: "+prob)
    prob
  }
  
  override def toString = theta.toList.toString()
  
  // NEW FUNCTIONS
  
  def updateTheta(doc: Map[String,Double], theta: HashMap[String,Double], step: Int, rel: Boolean) = {
    vectorAdd(scalarMultVector(1.0/step.toDouble, deltaTheta(doc,theta,rel)), theta)
  }
  
  // HELPER FUNCTIONS
  
  protected def innerProduct(v1: Map[String,Double], v2: HashMap[String,Double]) : Double = {
    (v1.keySet & v2.keySet).map(k => (v1.getOrElse(k, 0.0) * v2.getOrElse(k, 0.0))).sum
  }
  
  private def probRelevant(dFeature: Map[String,Double], theta: HashMap[String,Double]) : Double = {
    1.0 / (1.0 + Math.exp(-1.0 * bias - innerProduct(dFeature,theta)))
  }
  
  protected def scalarMultVector(scalar: Double, vector: Map[String,Double]) : Map[String,Double] = {
    vector.mapValues(i => i*scalar)
  }
  
  // Maybe not necessary
  protected def scalarMultVector(scalar: Double, vector: HashMap[String,Double]) : Map[String,Double] = {
    vector.map(kv => ((kv._1 -> (kv._2 * scalar)))).toMap
  }
  
  protected def vectorAdd(v1: Map[String,Double], theta: HashMap[String,Double]) = {
    for(key <- (v1.keySet)){
      theta(key) = theta.getOrElseUpdate(key, 0.0) + v1(key)
    }
  }
  
  protected def vectorAddImmut(v1: Map[String,Double], v2: Map[String,Double]) : Map[String,Double] = {
    (v1.keySet ++ v2.keySet).map(k => ((k -> (v1.getOrElse(k, 0.0) + v2.getOrElse(k, 0.0))))).toMap
  }
  
  private def deltaTheta(dFeature: Map[String,Double], theta: HashMap[String,Double], rel: Boolean) : Map[String,Double] = {
    if(rel){
      scalarMultVector(1.0 - probRelevant(dFeature,theta), dFeature)
    }else{
      scalarMultVector(-1.0*probRelevant(dFeature,theta), dFeature)
    }
  }

}