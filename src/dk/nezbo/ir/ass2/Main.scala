package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.ReutersCorpusIterator
import ch.ethz.dal.classifier.processing.XMLDocument
import ch.ethz.dal.classifier.processing.Tokenizer
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.FileWriter

object Main {
  
  //val TO_TRAIN = Int.MaxValue 
  //val TO_TEST = Int.MaxValue
  //val TO_VERIFY = Int.MaxValue
  
  val TO_TRAIN = 25000
  val TO_TEST = 5000
  val TO_VERIFY = 5000
  val ITERATIONS = 2
  
  var cfs : Map[String,Int] = null

  def main(args: Array[String]): Unit = {
    val METHOD = args(0)
    
    // load topics and definitions
    val topicDefs : Map[String,Set[String]] = loadTopics()
    
    println(topicDefs)
    
    // Load documents for transformation
    println("> Preparing Training data")
    val iter = new ReutersCorpusIterator("./reuter/data/train").take(TO_TRAIN)
    
    // Initialize classifiers (per topic)
    val classifiers = getClassifiers(METHOD, topicDefs, iter)
    
    // For each classifier (in parrallel) do x iterations with
    // their (previously given) training data
    println("> Training classifiers")
    for(clf <- classifiers.par){
      for(i <- (1 to ITERATIONS)){
        clf.train(i)
        println("\t["+clf.getTopic+"] Iteration "+i+" done.")
      }
    }
    
    // Evaluate on known documents
    var i = 0
    var tp = 0
    var tn = 0
    var fp = 0
    var fn = 0
    println()
    println("> Preparing Test data")
    
    val test_docs = new ReutersCorpusIterator("./reuter/data/test-with-labels").take(TO_TEST)
    var file = new File("reuter/results/classify-emil-jacobsen-l-"+METHOD+".run")
    file.getParentFile().mkdir()
    var fw = new FileWriter(file,true)
    
    // For statistics
    var prec = 0.0
    var rec = 0.0
    var f1sc = 0.0
    
    println("> Test set comparrison started.")
    for(doc <- test_docs){
    	if(i % 1 == 0)
    	  println("\t"+i+" documents processed.")
      
    	var all = new ListBuffer[String]()
    	for(cl <- classifiers){
    		val choice = cl.classify(doc)
    		if(choice) all += cl.getTopic
    		
    		val reality = doc.topics.contains(cl.getTopic)
    		def addResult(combo:(Boolean,Boolean)) = combo match {
    		  case (true,true) => tp = tp + 1
    		  case (true,false) => fp = fp + 1
    		  case (false,true) => fn = fn + 1
    		  case (false,false) => tn = tn + 1
    		}
    		addResult(choice,reality)
    	}

    	// print comparrison
    	val foundSet = all.toSet
    	prec = prec + precision(foundSet,doc.topics)
    	rec = rec + recall(foundSet,doc.topics)
    	f1sc = f1sc + f1score(prec,rec)
    	
    	fw.write(doc.ID+" ")
    	for(clazz <- all){
    		fw.write(clazz+" ")
    	}
    	fw.write("\n")
    	
    	i = i + 1
    }
    fw.write((prec/i.toDouble)+" "+(rec/i.toDouble)+" "+(f1sc/i.toDouble)+"\n")
    fw.close()
    
    val total = (tp + tn + fp + fn).toDouble
    println("\nEvaluations: TP="+tp+" TN="+tn+" FP="+fp+" FN="+fn)
    println("accuracy (TP+TN/tot)="+((tp+tn).toDouble/total))
    println("sensitivity (TP/TP+FN)="+(tp.toDouble/(tp.toDouble+fn.toDouble)))
    println("fall-out (FP/FP+TN)="+(fp.toDouble/(fp.toDouble+tn.toDouble)))
    
    // Classify unknown documents for all topics
    println("> Preparing Validation data")
    val verification_docs = new ReutersCorpusIterator("./reuter/data/test-without-labels").take(TO_VERIFY)
    file = new File("reuter/results/classify-emil-jacobsen-u-"+METHOD+".run")
    file.getParentFile().mkdir()
    fw = new FileWriter(file,true)
    
    println("> Validation set classification started.")
    i = 0
    for(doc <- verification_docs){
        if(i % 1000 == 0)
    	  println("\t"+i+" documents processed.")
    	  
    	var all = new ListBuffer[String]()
    	for(cl <- classifiers){
    		val choice = cl.classify(doc)
    		if(choice) all += cl.getTopic
    	}
    	
    	// print comparrison
    	fw.write(doc.ID+" ")
    	for(clazz <- all){
    		fw.write(clazz+" ")
    	}
    	fw.write("\n")
    	
    	i = i + 1
    }
    fw.close()
    println("> Everything done.")
  }
  
  def f1score(prec: Double, recall: Double) : Double = {
    if(prec+recall < 0.0001){
      return 0.0
    }
    2.0 * (prec*recall) / (prec+recall)
  }
  
  def precision[T](found: Set[T], relevant: Set[T]) : Double = {
    if(found.size == 0){
      0.0
    }
    found.intersect(relevant).size.toDouble / found.size.toDouble
  }
  
  def recall[T](found: Set[T], relevant: Set[T]) : Double = {
    if(relevant.size == 0){
      0.0
    }
    found.intersect(relevant).size.toDouble / relevant.size.toDouble
  }
  
  def getClassifiers(method: String, topics: Map[String,Set[String]], iter: Iterator[XMLDocument]) : Iterable[Classifier] = {
	if(method.equals("lr")){
	    // get idf values (for topic descriptions)
	    cfs = calculateCFS(topics)
	    val train : List[(Set[String],Array[Double])] = iter.map(d => ((d.topics, lrFeatures(d))) ).toList
	    return topics.keys.map(t => new LogisticRegression(t,train)).toList
	}else if(method.equals("nb")){
		// divide docs tfs to different topics by reference, without duplicates
		val map = scala.collection.mutable.HashMap.empty[String,ListBuffer[(Map[String,Int],Int)]]
		var totDocs = 0
		for(doc <- iter){
		  totDocs = totDocs + 1
		  val tfs = Utilities.getTermFrequencies(doc)
		  val tuple = ((tfs,doc.tokens.length))
		  for(topic <- doc.topics){
		    if(!map.contains(topic))
		      map(topic) = new ListBuffer[(Map[String,Int],Int)]
		    map(topic) += tuple
		  }
		}
		
		// give correct set of docs to appropriate classifiers
		return map.map(kv => new NaiveBayes(kv._1, kv._2, totDocs)).toList
	}
	List() // PLEASE DONT GO HERE :P
  }
  
  def calculateCFS(topics: Map[String,Set[String]]) = {
    var iter = new ReutersCorpusIterator("./reuter/data/train").take(TO_TRAIN)
    var num_docs = 0
    val keywords = topics.flatMap(t => t._2).toSet
    println(keywords)
    
    val cfs = scala.collection.mutable.Map[String, Int]()
    println("> Creating CFs")
    for(doc <- iter){
      num_docs = num_docs + 1
      cfs ++= Utilities.getTermFrequencies(doc).filter(t => keywords.contains(t._1)).map(c => (c._1  -> (1 + cfs.getOrElse(c._1, 0))))
      
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
    val tfs = Utilities.getTermFrequencies(doc)
	cfs.map(kv => Math.max(0.0 ,Main.tfidf(tfs.getOrElse(kv._1,0),kv._2, 200000))).toArray
  }

}