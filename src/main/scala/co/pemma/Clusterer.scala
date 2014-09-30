package main.scala.co.pemma

import java.util

import cc.factorie.app.nlp
import cc.factorie.app.nlp.embeddings.{EmbeddingDistance, EmbeddingOpts, SkipGramNegSamplingEmbeddingModel}
import cc.factorie.la.DenseTensor1
import co.pemma.embeddings.{WordVectorUtils, WordVectorMath}
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

  def documentCentroids(wordTensors : Iterable[DenseTensor1], wordVectors : WordVectorMath, K : Int, iterations : Int)
  : Seq[DenseTensor1] =
  {
    val vecSize = wordTensors.head.size
    val attributeList = new util.ArrayList[Attribute](vecSize + 1)
    for (i <- 0 to vecSize - 1)
      attributeList.add(new Attribute(i + ""))

    val instances = new Instances("", attributeList, wordTensors.size)
    wordTensors.foreach(t => {
      instances.add(new DenseInstance(1, t.toArray))
    })

    // run kmeans with the weka
    val kmeans = new SimpleKMeans()
    kmeans.setPreserveInstancesOrder(true);
    kmeans.setNumClusters(K)
    kmeans.setMaxIterations(iterations)
    kmeans.buildClusterer(instances)

    kmeans.getClusterCentroids.map(c => new DenseTensor1(c.toDoubleArray))
  }
}
