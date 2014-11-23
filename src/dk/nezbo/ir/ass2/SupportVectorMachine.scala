package dk.nezbo.ir.ass2

import scala.collection.mutable.HashMap

class SupportVectorMachine(topic: String, trainSet: Iterable[(Set[String],Map[String,Double])]) 
	extends LogisticRegression(topic: String, trainSet: Iterable[(Set[String],Map[String,Double])]) {
  
  //this.debugPrint = true

  override def updateTheta(doc: Map[String,Double], theta: HashMap[String,Double], step: Int, rel: Boolean) = {
    val lambda = 1.0
    val thetaShrink = scalarMultVector(1-1.0/step.toDouble, theta) // WARNING
    val positive = if(rel) 1 else -1
    val margin = 1.0 - positive * this.innerProduct(doc, theta)
    
    if(margin <= 0){
      this.vectorAdd(thetaShrink, theta)
    }else {
      this.vectorAdd(this.vectorAddImmut(thetaShrink,this.scalarMultVector(1.0 / (lambda * step)*positive,doc)),theta)
    }
  }
  
  protected def vectorAddImmut(v1: Map[String,Double], v2: Map[String,Double]) : Map[String,Double] = {
    (v1.keySet ++ v2.keySet).map(k => ((k -> (v1.getOrElse(k, 0.0) + v2.getOrElse(k, 0.0))))).toMap
  }
  
  // Maybe not necessary
  protected def scalarMultVector(scalar: Double, vector: HashMap[String,Double]) : Map[String,Double] = {
    vector.map(kv => ((kv._1 -> (kv._2 * scalar)))).toMap
  }
}