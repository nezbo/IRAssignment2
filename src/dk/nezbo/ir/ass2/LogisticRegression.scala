package dk.nezbo.ir.ass2

import scala.collection.immutable.Set
import ch.ethz.dal.classifier.processing.XMLDocument
import scala.util.Random
import scala.collection.mutable.HashMap

object LogisticRegression{
  val classifyCache = new HashMap[Int, Map[String,Double]]()
}

class LogisticRegression(topic: String, var trainPos: Seq[Map[String,Double]], var trainNeg: Seq[Map[String,Double]]) extends Classifier(topic:String) {
  
  // FIELDS
  
  val rand = new Random
  val bias = 1.0
  protected var debugPrint = false
  
  var theta: HashMap[String,Double] = HashMap[String,Double]()
  
  // INHERITED FUNCTIONS
  
  def train(iteration: Int) : Unit = {
	val positive = rand.nextBoolean
    val doc = if(positive) trainPos(rand.nextInt(trainPos.size)) else trainNeg(rand.nextInt(trainNeg.size))
    
	updateTheta(doc, theta, iteration, positive)	    
  }
  def classify(doc: XMLDocument) : Double = {
    val prob = probRelevant(LogisticRegression.classifyCache.getOrElseUpdate(doc.ID, Main.lrFeatures(doc)),theta)
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
  
  protected def vectorAdd(v1: Map[String,Double], theta: HashMap[String,Double]) = {
    for(key <- (v1.keySet)){
      theta(key) = theta.getOrElseUpdate(key, 0.0) + v1(key)
    }
  }
  
  private def deltaTheta(dFeature: Map[String,Double], theta: HashMap[String,Double], rel: Boolean) : Map[String,Double] = {
    if(rel){
      scalarMultVector(1.0 - probRelevant(dFeature,theta), dFeature)
    }else{
      scalarMultVector(-1.0*probRelevant(dFeature,theta), dFeature)
    }
  }

}