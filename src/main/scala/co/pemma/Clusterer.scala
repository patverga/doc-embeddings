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
object Clusterer  extends  App
{
  // these defaults are broken - get reset to 0 at runtime, not sure why
  val numClusters = 35
  val numIterations = 250

  def documentCentroids(doc : Iterable[String], wordVectors : WordVectorMath, K : Int, iterations : Int)
  : Seq[DenseTensor1] =
  {
    // split the doc into words
    val words = doc.filter(w => !nlp.lexicon.StopWords.containsWord(w)  && w.size > 1)
    // turn each word into an embedding vector
    print("Converting fac vectors to weka instances...")
    val wordTensors = for ( w <- words ; wv = wordVectors.phrase2Vec(w); if wv != null)
    yield (w, wv)

    val kmeans = clusterDocument(wordTensors, K, iterations)
    println("converting weka centroids to fac vectors..")
    kmeans.getClusterCentroids.map(c => {
      new DenseTensor1(c.toDoubleArray)
    })
//    val centroidWords = centroidTensors.map(c =>{
//      val w = wordVectors.nearestNeighbors(Array(), c, 1).head._1
//      println(w)
//      w
//    }).mkString(" ")
  }

  def clusterDocument(wordTensors: Iterable[(String, DenseTensor1)], K : Int, iterations : Int)
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
    kmeans.setNumClusters(K)
    kmeans.setMaxIterations(iterations)
    kmeans.buildClusterer(instances)

    kmeans
  }

  def centroidsToWords(kmeans : SimpleKMeans, wordVectors : WordVectorMath)
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
