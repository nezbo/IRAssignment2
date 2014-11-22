package dk.nezbo.ir.ass2

import scala.collection.mutable.HashMap

class SupportVectorMachine(topic: String, trainSet: Seq[(Set[String],Map[String,Double])]) 
	extends LogisticRegression(topic: String, trainSet: Seq[(Set[String],Map[String,Double])]) {

  override def updateTheta(doc: Map[String,Double], theta: HashMap[String,Double], step: Int, rel: Boolean) = {
    val lambda = 1.0
    val thetaShrink = this.scalarMultVector(1-1.0/step.toDouble, theta) // WARNING
    val positive = if(rel) 1 else -1
    val margin = 1.0 - positive * this.innerProduct(doc, theta)
    
    if(margin <= 0){
      this.vectorAdd(thetaShrink, theta)
    }else {
      this.vectorAdd(this.vectorAddImmut(thetaShrink,this.scalarMultVector(1.0 / (lambda * step)*positive,doc)),theta)
    }
  }
}