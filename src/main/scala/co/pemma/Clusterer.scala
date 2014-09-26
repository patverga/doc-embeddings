package main.scala.co.pemma

import java.util

import cc.factorie.app.nlp
import cc.factorie.app.nlp.embeddings.{EmbeddingDistance, EmbeddingOpts, SkipGramNegSamplingEmbeddingModel}
import cc.factorie.la.DenseTensor1
import co.pemma.embeddings.WordVectorMath
import weka.clusterers.SimpleKMeans
import weka.core.{Attribute, DenseInstance, Instances}

import scala.collection.JavaConversions._


/**
 * Created by pv on 8/28/14.
 */
object Clusterer  extends  App{
  val numClusters = 35
  val numIterations = 250

  def clusterWordVectors(doc :String, wordVectors : WordVectorMath, K : Int = numClusters, iterations : Int = numIterations)
  {
    // split the doc into words
    val words = doc.split("\\s+|\\n").filter(w => !nlp.lexicon.StopWords.containsWord(w)  && w.size > 1)
    // turn each word into an embedding vector
    print("Converting fac vectors to weka instances...")
    val wordTensors = for ( w <- words ; wv = wordVectors.word2Vec(w); if wv != null)
    yield (w, wordVectors.word2Vec(w))

    val kmeans = clusterWords( doc, wordTensors)
    kmeans.getClusterCentroids.foreach(c => {
      val centroidTensor = new DenseTensor1(c.toDoubleArray)
      println(wordVectors.nearestNeighbors(Array(), centroidTensor, 1).head._1)
    })
//    closestCentroid(kmeans, wordTensors, "software piracy")
//    sumWords(doc, wordTensors)

//    println(doc)
  }

  def clusterWords(doc: String, wordTensors: Array[(String, DenseTensor1)], K : Int = numClusters, iterations : Int = numIterations) 
  : SimpleKMeans =
  {
    val vecSize = wordTensors.head._2.size
    val attributeList = new util.ArrayList[Attribute](vecSize + 1)
    for (i <- 0 to vecSize - 1)
      attributeList.add(new Attribute(i + ""))

    val instances = new Instances("", attributeList, wordTensors.size)
    wordTensors.foreach(t => {
      instances.add(new DenseInstance(1, t._2.toArray))
    })
    println(" Done.")

    // run kmeans with the weka
    println("Running KMeans")
    val kmeans = new SimpleKMeans()
    kmeans.setPreserveInstancesOrder(true);
    kmeans.setNumClusters(numClusters)
    kmeans.setMaxIterations(numIterations)
    kmeans.buildClusterer(instances)

    kmeans
  }

  def clusterCentroidsToWords(kmeans : SimpleKMeans, wordVectors : WordVectorMath)
  {
    val words = kmeans.getClusterCentroids.flatMap(centroid => {
      val tensor = new DenseTensor1(centroid.toDoubleArray)
      val centroidWords = wordVectors.nearestNeighbors(Array(), tensor, 1)
      centroidWords.map(_._1)
    }).mkString(", ")
    println ("CLUSTER WORDS")
    println(words)
  }

  def closestCentroid(kmeans : SimpleKMeans, words: Array[(String, DenseTensor1)], query : String, wordVectors: WordVectorMath)
  {
    val queryTensor = new DenseTensor1(words.head._2.size, 0)
    var queryWordsUsed = 0
    query.split("\\s+").foreach(word => {
      val wordTensor = wordVectors.word2Vec(word)
      if (wordTensor != null) {
        queryTensor.+=(wordTensor)
        queryWordsUsed += 1
      }
    })
    queryTensor./=(queryWordsUsed)

    val clusterSimilarities = for(i <- 0 to kmeans.numberOfClusters()-1) yield
    {
      val centroid = kmeans.getClusterCentroids.get(i)
      val centroidTensor = new DenseTensor1(centroid.toDoubleArray)
      (centroidTensor.cosineSimilarity(queryTensor), i)
    }
    val mostSimilarCluster = clusterSimilarities.max
    val assignments = kmeans.getAssignments
    val clusterWords = for (i <- 0 to words.size-1 if assignments(i) == mostSimilarCluster._2)
    yield words(i)._1

    clusterWords.foreach(w => println(w))
  }

  def sumWords(doc: String, wordTensors: Array[(String, DenseTensor1)], wordVectors : WordVectorMath)
  {
    val embedding_in = new DenseTensor1(wordTensors.head._2.size, 0)
    val words_in = wordTensors.map(word =>
    {
      embedding_in.+=(word._2)
      word._1
    })
    embedding_in./=(wordTensors.size)
    val words = wordVectors.nearestNeighbors(words_in, embedding_in, numClusters).map(_._1).mkString(", ")
    println("SUM WORDS")
    println(words)
  }

}
