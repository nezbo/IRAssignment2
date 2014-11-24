package dk.nezbo.ir.ass2

import scala.collection.mutable.HashMap

class SupportVectorMachine(topic: String, trainSet: Iterable[(Set[String],Map[String,Double])]) 
	extends LogisticRegression(topic: String, trainSet: Iterable[(Set[String],Map[String,Double])]) {
  
  //this.debugPrint = true

  override def updateTheta(doc: Map[String,Double], theta: HashMap[String,Double], step: Int, rel: Boolean) = {
    val lambda = 1.0
    val isClass = if(rel) 1 else -1
    val innerProduct = doc.map(kv => kv._2 * theta.getOrElse(kv._1, 0.0)).sum
    
    val multBy = 1.0 - 1.0 / step.toDouble
    // shrinking
    theta ++= theta.map(kv => kv._1 -> kv._2 * multBy)
    
    val margin = 1.0 - isClass * innerProduct
    if(margin > 0){
      val factor = (1.0 / (lambda * step)) * isClass
      theta ++= doc.map(kv => kv._1 -> (theta.getOrElse(kv._1, 0.0) + kv._2 * factor))
    }
  }
  
  /*protected def vectorAddImmut(v1: Map[String,Double], v2: Map[String,Double]) : Map[String,Double] = {
    (v1.keySet ++ v2.keySet).map(k => ((k -> (v1.getOrElse(k, 0.0) + v2.getOrElse(k, 0.0))))).toMap
  }
  
  // Maybe not necessary
  protected def scalarMultVector(scalar: Double, vector: HashMap[String,Double]) : Map[String,Double] = {
    vector.map(kv => ((kv._1 -> (kv._2 * scalar)))).toMap
  }*/
}