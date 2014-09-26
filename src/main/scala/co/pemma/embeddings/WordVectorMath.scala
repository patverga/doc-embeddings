package co.pemma.embeddings

import cc.factorie.app.nlp.embeddings.TensorUtils
import cc.factorie.la.DenseTensor1

import scala.collection.mutable


/**
 * Created by pat on 9/26/14.
 */

class WordVectorMath(embedding : WordVectors){
  var threshold = embedding.threshold
  var vocab = embedding.vocab
  var weights = embedding.weights
  var D = embedding.D
  var V = embedding.V
  var K = 20

  def interactiveNearestNeighbor(): Unit = {

    while (true) {
      print("Enter word (EXIT to break) : ")
      val words = readLine().stripLineEnd.split(' ')
      val wordIds = words.map(word => getID(word)).filter(id => id != -1)

      if (wordIds.size == 0) {
        println("words not in vocab")
      }
      else if (words.size == 1 && words(0) == "EXIT") {
        return
      } else {
        // sum the input word vectors
        val embedding_in = new DenseTensor1(D, 0)
        wordIds.foreach(wordId => embedding_in.+=(weights(wordId)))
        embedding_in./=(wordIds.size)
        val pq = nearestNeighbors(words, embedding_in, K)
        val arr = new Array[(String, Double)](pq.size)
        var i = 0
        while (pq.nonEmpty) { // min heap
          arr(i) = (pq.head._1, pq.head._2)
          i += 1
          pq.dequeue()
        }
        println("\t\t\t\t\t\tWord\t\tCosine Distance")
        arr.reverse.foreach(x => println("%50s\t\t%f".format(x._1, x._2)))

      }
    }
  }

  def nearestNeighbors(words: Array[String], embedding_in: DenseTensor1, k : Int)
  : mutable.PriorityQueue[(String, Double)] = {
    val pq = new mutable.PriorityQueue[(String, Double)]()(dis())
    // find knn to the resulting vector
    for (i <- 0 until vocab.size) if (words.size != 1 || !words(0).equals(vocab(i))) {
      val embedding_out = weights(i)
      val score = TensorUtils.cosineDistance(embedding_in, embedding_out)
      if (i < k) pq.enqueue(vocab(i) -> score)
      else if (score > pq.head._2) {
        // if the score is greater the min, then add to the heap
        pq.dequeue()
        pq.enqueue(vocab(i) -> score)
      }
    }
    pq
  }

  // private helper functions
  private def dis() = new Ordering[(String, Double)] {
    def compare(a: (String, Double), b: (String, Double)) = -a._2.compare(b._2)
  }
  private def getID(word: String): Int = {
    for (i <- 0 until vocab.length) if (vocab(i).equalsIgnoreCase(word))
      return i
    -1
  }
  def word2Vec(word:String):DenseTensor1 =
  {
    val id = getID(word)
    if (id != -1)
      weights(id)
    else
      null
  }
}

object TestDistance extends App{
  val serialLocation = "./vectors/serial-vectors"
  WordVectorsSerialManager.vectorTxt2Serial("./vectors/google-vectors", serialLocation)
  val distance = new WordVectorMath(WordVectorsSerialManager.deserialize(serialLocation))
  distance.interactiveNearestNeighbor()
}




