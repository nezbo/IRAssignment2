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
  
  val TRAIN_FOLDER = "./reuter/data/train"
  val TEST_FOLDER = "./reuter/data/test-with-labels"
  val VALIDATION_FOLDER = "./reuter/data/test-without-labels"
  
  val ITERATIONS = 250000
  
  // Will be assigned in "preprocessing"
  var dfs : Map[String,Int] = null
  var topics : Set[String] = null
  var numDocs : Int = 0

  /**
   * The main method where all the magic happens. The first
   * console argument is will be interpreted as the wanted
   * classification method:
   * "nb" = Naive Bayes
   * "lr" = Logistic Regression
   * "svm" = Support Vector Machine
   */
  def main(args: Array[String]): Unit = {
    StopWatch.start
    val METHOD = args(0)
    
    println("> Preprocessing (topics,dfs)")
    
    // Preprocess (gather topics and calculate dfs')
    preprocess(METHOD)
    
    StopWatch.tick
    
    println("Topics: "+topics)
    
    println("> Preparing Training data")
    
    // Initialize classifiers (per topic)
    val classifiers = getClassifiers(METHOD)
    
    StopWatch.tick
    
    // For each classifier (in parrallel) do X iterations with
    // their (previously given) training data
    println("> Training classifiers")
    classifiers.train(ITERATIONS)
    
    StopWatch.tick
    
    // Evaluate on known documents
    var i = 0
    var tp = 0
    var tn = 0
    var fp = 0
    var fn = 0
    println()
    println("> Preparing Test data")
    
    val test_docs = new ReutersCorpusIterator(TEST_FOLDER)
    val sb = new StringBuilder()
    
    StopWatch.tick
    
    // For statistics in output file
    var prec = 0.0
    var rec = 0.0
    var f1sc = 0.0
    
    println("> Test set comparrison started.")
    var doc : XMLDocument = null
    while(test_docs.hasNext){
    	doc = test_docs.next
    	
    	if(i % 10000 == 0)
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

    	// Add precision, recall and f1 score to the sums
    	val _prec = precision(classified,doc.topics)
    	val _rec = recall(classified,doc.topics)
    	prec = prec + _prec
    	rec = rec + _rec
    	f1sc = f1sc + f1score(_prec,_rec)
    	
    	// Cache output line to be written to file later
    	sb.append(doc.ID+" ")
    	for(clazz <- classified){
    		sb.append(clazz+" ")
    	}
    	sb.append("\n")
    	
    	i = i + 1
    }
    // My own small statistics
    val total = (tp + tn + fp + fn).toDouble
    println("\nEvaluations: TP="+tp+" TN="+tn+" FP="+fp+" FN="+fn)
    println("accuracy (TP+TN/tot)="+((tp+tn).toDouble/total))
    println("sensitivity (TP/TP+FN)="+(tp.toDouble/(tp.toDouble+fn.toDouble)))
    println("fall-out (FP/FP+TN)="+(fp.toDouble/(fp.toDouble+tn.toDouble)))
    
    StopWatch.tick
    
    // Print everything to file
    var file = new File("reuter/results/classify-emil-jacobsen-l-"+METHOD+".run")
    file.getParentFile().mkdir()
    var fw = new FileWriter(file,true)
    fw.write((prec/i.toDouble)+" "+(rec/i.toDouble)+" "+(f1sc/i.toDouble)+"\n")
    fw.write(sb.toString)
    fw.close()
    
    // Classify unknown documents for all topics
    println("> Preparing Validation data")
    val verification_docs = new ReutersCorpusIterator(VALIDATION_FOLDER)
    file = new File("reuter/results/classify-emil-jacobsen-u-"+METHOD+".run")
    file.getParentFile().mkdir()
    fw = new FileWriter(file,true)
    
    StopWatch.tick
    
    println("> Validation set classification started.")
    i = 0
    while(verification_docs.hasNext){
    	doc = verification_docs.next
    	
        if(i % 1000 == 0)
    	  println("\t"+i+" documents processed.")
    	  
    	var all = classifiers.classify(doc)
    	
    	// Write classifications to file
    	fw.write(doc.ID+" ")
    	for(clazz <- all){
    		fw.write(clazz+" ")
    	}
    	fw.write("\n")
    	
    	i = i + 1
    }
    fw.close()
    println("> Everything done.")
    
    StopWatch.tick
    StopWatch.stop
  }
  
  /**
   * Performs some (necessary) pre-processing before loading and actually
   * handling the documents. That is calculating total number of documents,
   * gathering all the possible topics, and (if necessary) aggregating dfs
   * values for use in tf-idf calculations.
   */
  def preprocess(method : String) = {
    val iter = new ReutersCorpusIterator(TRAIN_FOLDER)
    val htopics = new HashSet[String]
    var tempdfs = scala.collection.mutable.Map[String, Int]()
    for(doc <- iter){
      // topics needed by all
      htopics ++= doc.topics
      
      // dfs (for calculating tf-idf
	  if(method.equals("svm") || method.equals("lr")){
		tempdfs ++= Utilities.getTermFrequencies(doc).map(c => (c._1  -> (1 + tempdfs.getOrElse(c._1, 0))))
	  }
      
      // logging total number of documents
      numDocs = numDocs + 1
    }
    
    dfs = tempdfs.toMap
    topics = htopics.toSet
  }
  
  /**
   * Transforms the iterator stream to a map which contains two
   * sets of documents for each topic. The first with all the
   * documents that contains the topic (positive set) and the
   * other with all those who doesn't (negative set). The
   * documents have been converted to the top 50 tf-idf values.
   */
  def docsToSets(iter: Iterator[XMLDocument]) : HashMap[String,(ListBuffer[Map[String,Double]],ListBuffer[Map[String,Double]])] = {
    val result = new HashMap[String,(ListBuffer[Map[String,Double]],ListBuffer[Map[String,Double]])]
    var i = 0
    for(doc <- iter){
      if(i % 10000 == 0) println(i+" docs loaded.")
      
      val features = lrFeatures(doc)
      for(topic <- topics){
        val tuple = result.getOrElseUpdate(topic, ((new ListBuffer[Map[String,Double]], new ListBuffer[Map[String,Double]])) )
        if(doc.topics.contains(topic)){
          tuple._1 += features
        }else{
          tuple._2 += features
        }
      }
      i = i + 1
    }
    
    result
  }
  
  /**
   * Creates the classifiers for the chosen classification method
   * (given by the argument).
   */
  def getClassifiers(method: String) : ClassifierMaster = {
    val iter = new ReutersCorpusIterator(TRAIN_FOLDER)
	if(method.equals("lr")){	    
	    val train = docsToSets(iter)
	    return new OverConstCM(0.5,topics.map(t => new LogisticRegression(t,train(t)._1, train(t)._2)).toList)
	}else if(method.equals("nb")){
		// divide docs tfs to different topics by reference, without duplicates
		val map = HashMap.empty[String,HashMap[String,Int]]
		val length = HashMap.empty[String,Int]
		val cDocs = HashMap.empty[String,Int]
		var totDocs = 0
		var vocabulary : HashSet[String] = HashSet()
		
		var doc : XMLDocument = null
		while(iter.hasNext){
		  doc = iter.next
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
		
		// give correct set of docs to appropriate classifiers
		// aggregated down to minimum data needed
		return new TopValueCM(3,map.keys.map(k => new NaiveBayes(k, map(k).toMap, length(k), cDocs(k), totDocs, vocabulary.size)))
	} else if(method.equals("svm")) {
	    val train = docsToSets(iter)
	    return new OverConstCM(0.5,topics.map(t => new LogisticRegression(t,train(t)._1, train(t)._2)).toList)
	}
	new TopValueCM(1,List()) // PLEASE DONT GO HERE :P
  }
  
  /**
   * Simply adds all the values of "other" to the "hashmap". Adding the values
   * if already present in the HashMap.
   */
  private def addAll(hashmap: HashMap[String,Int], other: Map[String,Int]) = {
	for(entry <- other){
		hashmap(entry._1) = hashmap.getOrElse(entry._1 , 0) + entry._2
	}
  }
  
  /**
   * Creates the features for a document needed for the LR
   * and SVM classification methods. I have chosen it to be
   * the top 50 tf-idf values for the given document (i.e. the
   * terms can vary between documents).
   */
  def lrFeatures(doc: XMLDocument) : Map[String,Double] = {
    val result = Utilities.getTermFrequencies(doc)
    		.map(t => ((t._1, Utilities.tfidf(t._2 , dfs.getOrElse(t._1, 0), numDocs) )) )
    		.toSeq
    		.sortBy(t => -t._2)
    		.take(50)
    		.toMap
    result
  }

}