package dk.nezbo.ir.ass2

import scala.collection.immutable.Set
import ch.ethz.dal.classifier.processing.XMLDocument
import scala.util.Random
import scala.collection.mutable.HashMap

/**
 * Static stuff for this class. The tf-idf features of
 * given documents to classify, so every classifier
 * doesn't have to transform it again. Not the most
 * smooth solution, sorry.
 */
object LogisticRegression{
  val classifyCache = new HashMap[Int, Map[String,Double]]()
}

/**
 * A specific classifier that uses logistic regression to find 
 * a feature vector which classifies the given topic well. The
 * class takes as arguments its assigned topic, a set of
 * positive training documents and a set of negative training
 * documents.
 */
class LogisticRegression(topic: String, var trainPos: Seq[Map[String,Double]], var trainNeg: Seq[Map[String,Double]]) extends Classifier(topic:String) {
  
  // FIELDS
  
  val rand = new Random
  val bias = 1.0
  
  var theta: HashMap[String,Double] = HashMap[String,Double]()
  
  // INHERITED FUNCTIONS
  
  def clearTrain = { trainPos = null; trainNeg = null}
  
  def train(iteration: Int) : Unit = {
	val positive = rand.nextBoolean // coin flip to pick pos/neg training set
    val doc = if(positive) trainPos(rand.nextInt(trainPos.size)) else trainNeg(rand.nextInt(trainNeg.size)) // take random doc from it
    
	updateTheta(doc, iteration, positive)	    
  }
  
  
  def classify(doc: XMLDocument) : Double = {
    // simply the final probability of the doc being relevant
    probRelevant(LogisticRegression.classifyCache.getOrElseUpdate(doc.ID, Main.lrFeatures(doc))) 
  }
  
  override def toString = theta.toList.toString()
  
  // NEW FUNCTIONS
  
  /**
   * Updates the theta, being a mutable map it doesn't need to return anything.
   * In this whole ordeal the sparse vectors are maps with the key as "index",
   * every other value is defined to be zero.
   */
  def updateTheta(doc: Map[String,Double], step: Int, rel: Boolean) = {
    addToTheta(scalarMultVector(1.0/Math.sqrt(step.toDouble), deltaTheta(doc,rel)))
  }
  
  // HELPER FUNCTIONS
  
  /**
   * Calculates the inner product of two vectors.
   */
  protected def innerProduct(v1: Map[String,Double], v2: HashMap[String,Double]) : Double = {
    (v1.keySet & v2.keySet).map(k => (v1.getOrElse(k, 0.0) * v2.getOrElse(k, 0.0))).sum
  }
  
  /**
   * Calculates the probability that a document (vector) is relevant.
   */
  private def probRelevant(dFeature: Map[String,Double]) : Double = {
    1.0 / (1.0 + Math.exp(-1.0 * bias - innerProduct(dFeature,theta)))
  }
  
  /**
   * Multiplies a vector by a scalar.
   */
  protected def scalarMultVector(scalar: Double, vector: Map[String,Double]) : Map[String,Double] = {
    vector.mapValues(i => i*scalar)
  }
  
  /**
   * Adds two vectors together. Specifically adds a vector to the mutable
   * theta.
   */
  protected def addToTheta(v1: Map[String,Double]) = {
    for(key <- (v1.keySet)){
      theta(key) = theta.getOrElseUpdate(key, 0.0) + v1(key)
    }
  }
  
  /**
   * Calculates delta theta (the difference to update by) depending on the
   * vector and if the originating document was relevant for this topic.
   */
  private def deltaTheta(dFeature: Map[String,Double], rel: Boolean) : Map[String,Double] = {
    if(rel){
      scalarMultVector(1.0 - probRelevant(dFeature), dFeature)
    }else{
      scalarMultVector(-1.0*probRelevant(dFeature), dFeature)
    }
  }

}