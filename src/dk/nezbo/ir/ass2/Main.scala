package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.ReutersCorpusIterator
import ch.ethz.dal.classifier.processing.XMLDocument
import ch.ethz.dal.classifier.processing.Tokenizer
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.FileWriter

object Main {
  
  val TO_TRAIN = Int.MaxValue 
  val ITERATIONS = 1
  
  var cfs : Map[String,Int] = null

  def main(args: Array[String]): Unit = {
    val CLASSIFIER = args(0)
    
    // load topics and definitions
    val topicDefs : Map[String,Set[String]] = loadTopics()
    
    println(topicDefs)
    
    // Load documents and transform to features,topics
    val iter = new ReutersCorpusIterator("./reuter/data/train").take(TO_TRAIN)
    
    // get idf values (for topic descriptions)
    cfs = calculateCFS(topicDefs)
    val train : List[(Set[String],Array[Double])] = iter.map(d => ((d.topics, lrFeatures(d))) ).toList
    
    // Initialize classifiers (per topic)
    val classifiers = topicDefs.keys.map(t => (t,new LogisticRegression(t,train)))
    
    // For each iteration, let all classifiers process 
    // their (previously given) training data
    for(i <- (1 to ITERATIONS)){
    	println("Starting Iteration "+i)

	    // Expose document to all classifiers
	    for(clf <- classifiers){
	    	clf._2.train(i)
	    	println("\t["+clf._1+"] Done")
	    }
    }
    
    // Evaluate on known documents
    var tp = 0
    var tn = 0
    var fp = 0
    var fn = 0
    val test_docs = new ReutersCorpusIterator("./reuter/data/test-with-labels")
    for(doc <- test_docs){
    	var all = new ListBuffer[String]()
    	for(cl <- classifiers){
    		val choice = cl._2.classify(doc)
    		if(choice) all += cl._1
    		
    		val reality = doc.topics.contains(cl._1)
    		def addResult(combo:(Boolean,Boolean)) = combo match {
    		  case (true,true) => tp = tp + 1
    		  case (true,false) => fp = fp + 1
    		  case (false,true) => fn = fn + 1
    		  case (false,false) => tn = tn + 1
    		}
    		addResult(choice,reality)
    	}
    	
    	// print comparrison
    	//println("Reality: "+doc.topics.toString+" Classified: "+all.toSet.toString)
    }
    val total = (tp + tn + fp + fn).toDouble
    println("\nEvaluations: TP="+tp+" TN="+tn+" FP="+fp+" FN="+fn)
    println("accuracy (TP+TN/tot)="+((tp+tn).toDouble/total))
    println("sensitivity (TP/TP+FN)="+(tp.toDouble/(tp.toDouble+fn.toDouble)))
    println("fall-out (FP/FP+TN)="+(fp.toDouble/(fp.toDouble+tn.toDouble)))
    
    // Classify unknown documents for all topics for all classifiers
    val verification_docs = new ReutersCorpusIterator("./reuter/data/test-without-labels")
    val file = new File("reuter/results/classify-emil-jacobsen-u-"+CLASSIFIER+".run")
    file.getParentFile().mkdir()
    val fw = new FileWriter(file,true)
    
    for(doc <- verification_docs){
    	var all = new ListBuffer[String]()
    	for(cl <- classifiers){
    		val choice = cl._2.classify(doc)
    		if(choice) all += cl._1
    	}
    	
    	// print comparrison
    	fw.write(doc.name+" ")
    	for(clazz <- all){
    		fw.write(clazz+" ")
    	}
    	fw.write("\n")
    }
  }
  
  def calculateCFS(topics: Map[String,Set[String]]) = {
    var iter = new ReutersCorpusIterator("./reuter/data/train").take(TO_TRAIN)
    var num_docs = 0
    val keywords = topics.flatMap(t => t._2).toSet
    println(keywords)
    
    val cfs = scala.collection.mutable.Map[String, Int]()
    println("CREATING CFS")
    for(doc <- iter){
      num_docs = num_docs + 1
      cfs ++= getTermFrequencies(doc).filter(t => keywords.contains(t._1)).map(c => (c._1  -> (1 + cfs.getOrElse(c._1, 0))))
      
      if(num_docs % 1000 == 0) println(num_docs + " docs handled")
    }
    println("cfs: "+cfs)
    cfs.toMap
  }
  
  def getTopicCounts(docs: List[XMLDocument]) : Map[String,Double] = {
	  docs.flatMap(d => d.topics).groupBy(identity).mapValues(v => v.length.toDouble / docs.length.toDouble)
  }
  
  def tfidf(tf: Int, df: Int, docs: Int) = {
    (1+Math.log10(tf)) * idf(df,docs)
  }
  
  def idf(df: Int, n: Int) = {
    Math.log(n) - Math.log(df)
  }
  
  def getTermFrequencies(doc: XMLDocument) : Map[String,Int] = {
    doc.tokens.map(Utilities.getStem(_)).groupBy(identity).mapValues(v => v.length)
  }
  
  def loadTopics() : Map[String,Set[String]] = {
    val file = scala.io.Source.fromFile("./reuter/topic_codes.txt")
    val lines = file.getLines
    
    val result = scala.collection.mutable.Map[String, Set[String]]()
    
    for(line <- lines){
      if(!line.startsWith(";") && line.length() > 5){
	      val split = line.split("\t")
	      result += (split(0) -> (Tokenizer.tokenize(split(1)).map(Utilities.getStem(_)).filter(t => t.length() > 1).toSet))
      }
    }
    result.toMap
  }
  
  def lrFeatures(doc: XMLDocument) : Array[Double] = {
    val tfs = getTermFrequencies(doc)
	cfs.map(kv => Math.max(0.0 ,Main.tfidf(tfs.getOrElse(kv._1,0),kv._2, 200000))).toArray
  }
}