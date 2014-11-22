package dk.nezbo.ir.ass2

import ch.ethz.dal.classifier.processing.ReutersCorpusIterator
import ch.ethz.dal.classifier.processing.XMLDocument
import ch.ethz.dal.classifier.processing.Tokenizer
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.FileWriter
import scala.collection.mutable.HashMap
import Utilities._
import scala.collection.mutable.HashSet

object Main {
  
  /*val TO_TRAIN = Int.MaxValue 
  val TO_TEST = Int.MaxValue
  val TO_VERIFY = Int.MaxValue*/
  
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
    for(i <- (1 to ITERATIONS)){
      classifiers.train(i)
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
    val sb = new StringBuilder()
    
    // For statistics
    var prec = 0.0
    var rec = 0.0
    var f1sc = 0.0
    
    println("> Test set comparrison started.")
    for(doc <- test_docs){
    	if(i % 1000 == 0)
    	  println("\t"+i+" documents processed.")
      
    	var classified = classifiers.classify(doc)
    	
    	val _tp = (classified &doc.topics).size
    	val _fp = (classified -- doc.topics).size
    	val _fn = (doc.topics -- classified).size
    	val _tn = classifiers.size - _tp - _fp - _fn
    	
    	
    	tp = tp + _tp
    	fp = fp + _fp
    	fn = fn + _fn
    	tn = tn + _tn

    	// print comparrison
    	val _prec = precision(classified,doc.topics)
    	val _rec = recall(classified,doc.topics)
    	prec = prec + _prec
    	rec = rec + _rec
    	f1sc = f1sc + f1score(_prec,_rec)
    	
    	sb.append(doc.ID+" ")
    	for(clazz <- classified){
    		sb.append(clazz+" ")
    	}
    	sb.append("\n")
    	
    	i = i + 1
    }
    val total = (tp + tn + fp + fn).toDouble
    println("\nEvaluations: TP="+tp+" TN="+tn+" FP="+fp+" FN="+fn)
    println("accuracy (TP+TN/tot)="+((tp+tn).toDouble/total))
    println("sensitivity (TP/TP+FN)="+(tp.toDouble/(tp.toDouble+fn.toDouble)))
    println("fall-out (FP/FP+TN)="+(fp.toDouble/(fp.toDouble+tn.toDouble)))
    
    // Print to file
    var file = new File("reuter/results/classify-emil-jacobsen-l-"+METHOD+".run")
    file.getParentFile().mkdir()
    var fw = new FileWriter(file,true)
    fw.write((prec/i.toDouble)+" "+(rec/i.toDouble)+" "+(f1sc/i.toDouble)+"\n")
    fw.write(sb.toString)
    fw.close()
    

    
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
    	  
    	var all = classifiers.classify(doc)
    	
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
      return 0.0
    }
    found.intersect(relevant).size.toDouble / found.size.toDouble
  }
  
  def recall[T](found: Set[T], relevant: Set[T]) : Double = {
    if(relevant.size == 0){
      return 0.0
    }
    found.intersect(relevant).size.toDouble / relevant.size.toDouble
  }
  
  def getClassifiers(method: String, topics: Map[String,Set[String]], iter: Iterator[XMLDocument]) : ClassifierMaster = {
	if(method.equals("lr")){
	    // get idf values (for topic descriptions)
	    cfs = calculateCFS(topics)
	    val train : List[(Set[String],Map[String,Double])] = iter.map(d => ((d.topics, lrFeatures(d))) ).toList
	    return new OverPointFiveCM(topics.keys.map(t => new LogisticRegression(t,train)).toList)
	}else if(method.equals("nb")){
		// divide docs tfs to different topics by reference, without duplicates
		val map = HashMap.empty[String,HashMap[String,Int]]
		val length = HashMap.empty[String,Int]
		val cDocs = HashMap.empty[String,Int]
		var totDocs = 0
		var vocabulary : HashSet[String] = HashSet()
		for(doc <- iter){
		  totDocs = totDocs + 1
		  
		  if(totDocs % 1000 == 0)
		    println("\t"+totDocs+" documents processed.")
		  
		  val tfs = Utilities.getTermFrequencies(doc)
		  vocabulary ++= tfs.keySet
		  val dLength = doc.tokens.length
		  for(topic <- doc.topics){
		    if(!map.contains(topic)){
		      map(topic) = new HashMap()
		    }
		    addAll(map(topic),tfs)
		    
		    // increment length and num docs
		    length(topic) = length.getOrElse(topic, 0) + dLength
		    cDocs(topic) = cDocs.getOrElse(topic, 0) + 1
		  }
		}
		
		println(vocabulary.size)
		
		// give correct set of docs to appropriate classifiers
		// aggregated down to minimum data needed
		return new TopValueCM(map.keys.map(k => new NaiveBayes(k, map(k).toMap, length(k), cDocs(k), totDocs, vocabulary.size)))
	}
	new OverPointFiveCM(List()) // PLEASE DONT GO HERE :P
  }
  
  private def addAll(hashmap: HashMap[String,Int], other: Map[String,Int]) = {
	for(entry <- other){
		hashmap(entry._1) = hashmap.getOrElse(entry._1 , 0) + entry._2
	}
  }
  
  def calculateCFS(topics: Map[String,Set[String]]) = {
    var iter = new ReutersCorpusIterator("./reuter/data/train").take(TO_TRAIN)
    var num_docs = 0
    
    val cfs = scala.collection.mutable.Map[String, Int]()
    println("> Creating CFs")
    for(doc <- iter){
      num_docs = num_docs + 1
      cfs ++= Utilities.getTermFrequencies(doc).map(c => (c._1  -> (1 + cfs.getOrElse(c._1, 0))))
      
      if(num_docs % 1000 == 0) println(num_docs + " docs handled - size: "+cfs.size)
    }
    //println("cfs: "+cfs)
    cfs.toMap
  }
  
  def getTopicCounts(docs: List[XMLDocument]) : Map[String,Double] = {
	  docs.flatMap(d => d.topics).groupBy(identity).mapValues(v => v.length.toDouble / docs.length.toDouble)
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
  
  def lrFeatures(doc: XMLDocument) : Map[String,Double] = {
    val result = Utilities.getTermFrequencies(doc).map(t => ((t._1, Utilities.tfidf(t._2 , cfs.getOrElse(t._1, 0), 200000) )) ).toSeq.sortBy(t => -t._2).take(50).toMap
    result
  }

}