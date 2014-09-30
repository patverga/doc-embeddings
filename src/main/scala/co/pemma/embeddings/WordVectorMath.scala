package co.pemma.embeddings

import cc.factorie.app.nlp.embeddings.TensorUtils
import cc.factorie.la.DenseTensor1
import org.apache.commons.lang3.text.WordUtils

import scala.collection.mutable


/**
 * Created by pat on 9/26/14.
 */

class WordVectorMath(embedding : WordVectors){
  var threshold = embedding.threshold
  var unigrams = embedding.unigrams
  var bigrams = embedding.bigrams
  var trigrams = embedding.trigrams
  var weights = embedding.weights
  var D = embedding.D
  var V = embedding.V
  var K = 20

  def interactiveNearestNeighbor(): Unit = {

    while (true) {
      print("Enter word (EXIT to break) : ")
      val words = readLine().stripLineEnd.split(' ')

      if (words.size == 1 && words(0) == "EXIT") {
        return
      } else {
        // sum the input word vectors
        val embedding_in = sumWords(words)
        if (embedding_in != null) {
          val pq = nearestNeighbors(words, embedding_in, K)
          val arr = new Array[(String, Double)](pq.size)
          var i = 0
          while (pq.nonEmpty) {
            // min heap
            arr(i) = (pq.head._1, pq.head._2)
            i += 1
            pq.dequeue()
          }
          println("\t\t\t\t\t\tWord\t\tCosine Distance")
          arr.reverse.foreach(x => println("%50s\t\t%f".format(x._1, x._2)))
        }
      }
    }
  }

  def nearestNeighbors(words: Array[String], embedding_in: DenseTensor1, k : Int)
  : mutable.PriorityQueue[(String, Double)] = {
    val pq = new mutable.PriorityQueue[(String, Double)]()(dis())
    // find knn to the resulting vector
    var offset = 0
    for (vocab <- Seq(unigrams, bigrams, trigrams)) {
      for (i <- 0 until vocab.length) if (words.size != 1 || !words(0).equals(vocab(i))) {
        val embedding_out = weights(i + offset)
        val score = TensorUtils.cosineDistance(embedding_in, embedding_out)
        if (i < k) pq.enqueue(vocab(i) -> score)
        else if (score > pq.head._2) {
          // if the score is greater the min, then add to the heap
          pq.dequeue()
          pq.enqueue(vocab(i) -> score)
        }
      }
      offset += vocab.length
    }
    pq
  }

  def sumWords(words: Iterable[String]) : DenseTensor1 = {
    val wordIds = words.map(word => getID(word)).filter(id => id != -1)
    if (wordIds.size == 0) {
      return null
    }
    val embedding_in = new DenseTensor1(D, 0)
    wordIds.foreach(wordId => embedding_in.+=(weights(wordId)))
    embedding_in./=(wordIds.size)
    embedding_in
  }

  def sumPhrases(words: Iterable[String]) : DenseTensor1 = {
    val embedding_in = new DenseTensor1(D, 0)
    var tensorsUsed = 0
    words.foreach(w => {
      val tensor = phrase2Vec(w)
      if (tensor != null)
      {
        tensorsUsed += 1
        embedding_in.+=(tensor)
      }
    })
    embedding_in./=(tensorsUsed)
    embedding_in
  }

  def word2Vec(word:String):DenseTensor1 =
  {
    val id = getID(word)
    if (id != -1)
      weights(id)
    else
      null
  }

  def phrase2Vec(phrase:String):DenseTensor1 =
  {
    // make various versions of the input phrase in order of priority
    val phraseUnderline = phrase.replaceAll(" ", "_")
    val phrasings = Set(WordUtils.capitalizeFully(phraseUnderline, '_'), phraseUnderline, phraseUnderline.toLowerCase, phrase, phrase.toLowerCase)
    phrasings.foreach(p => {
      // if this creates a tensor, return it
      val tensor = sumWords(p.split("\\s+"))
      if (tensor != null) {
//        println(s"found tensor for $p")
        return tensor
      }
    })
    println(s"words not in vocab : $phrase")
    null
  }

  // private helper functions
  private def dis() = new Ordering[(String, Double)] {
    def compare(a: (String, Double), b: (String, Double)) = -a._2.compare(b._2)
  }

  private def getID(word: String): Int = {
    var offset = 0
    val underscoreCount = word.length() - word.replace("_", "").length()
    val vocab = if (underscoreCount == 0)
      unigrams
    else if (underscoreCount == 1) {
      offset = unigrams.length
      bigrams
    }
    else if (underscoreCount == 2) {
      offset = unigrams.length + bigrams.length
      trigrams
    }
    else // vocab only supports up to trigrams
      return -1
    for (i <- 0 until vocab.length) if (vocab(i).equals(word))
      return i+offset
    -1
  }
}

object TestDistance extends App{
//  val inLocation = "./vectors/newswire-vectors.txt"
//  val outLocation = "./vectors/newswire-vectors.dat"

  val inLocation = "./vectors/google-vectors"
  val outLocation = "./vectors/serial-vectors"

//  WordVectorsSerialManager.vectorTxt2Serial(inLocation, outLocation)
  val distance = new WordVectorMath(WordVectorsSerialManager.deserialize(outLocation))
//  println(distance.phrase2Vec("bill clinton"))
  distance.interactiveNearestNeighbor()

}




