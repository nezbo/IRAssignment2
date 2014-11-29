package dk.nezbo.ir.ass2

import scala.collection.mutable.HashMap

/**
 * The Support Vector Machine (SVM) is a subclass of Logistic Regression
 * since it simply changes the gradient descent method.
 */
class SupportVectorMachine(topic: String, trainPos: Seq[Map[String,Double]], trainNeg: Seq[Map[String,Double]]) 
	extends LogisticRegression(topic: String, trainPos: Seq[Map[String,Double]], trainNeg: Seq[Map[String,Double]]) {

  override def updateTheta(doc: Map[String,Double], step: Int, rel: Boolean) = {
    val lambda = 1.0
    val isClass = if(rel) 1 else -1
    val innerProduct = doc.map(kv => kv._2 * theta.getOrElse(kv._1, 0.0)).sum
    
    val multBy = 1.0 - 1.0 / step.toDouble
    // shrinking
    theta ++= theta.map(kv => kv._1 -> kv._2 * multBy)
    
    val margin = 1.0 - isClass * innerProduct
    if(margin > 0){
      val factor = (1.0 / (lambda * Math.sqrt(step))) * isClass
      theta ++= doc.map(kv => kv._1 -> (theta.getOrElse(kv._1, 0.0) + kv._2 * factor))
    }
  }
}