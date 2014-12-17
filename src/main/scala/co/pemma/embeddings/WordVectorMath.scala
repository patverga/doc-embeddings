package co.pemma.embeddings

import cc.factorie.app.nlp
import cc.factorie.app.nlp.embeddings.TensorUtils
import cc.factorie.la.{Tensor, DenseTensor1}
import org.apache.commons.lang3.StringUtils
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
          val nn = nearestNeighbors(words, embedding_in, K)
          println("\t\t\t\t\t\tWord\t\tCosine Distance")
          nn.reverse.foreach(x => println("%50s\t\t%f".format(x._1, x._2)))
        }
      }
    }
  }

  def nearestNeighbors(words: Array[String], embedding_in: DenseTensor1, k : Int)
  : Seq[(String, Double)] = {
    val pq = new mutable.PriorityQueue[(String, Double)]()(dis())
    if (embedding_in != null) {
      // find knn to the resulting vector
      for (vocab <- Seq(unigrams.iterator, bigrams.iterator, trigrams.iterator)) {
        while (vocab.hasNext) {
          val (word, i) = vocab.next()
          if (words.size != 1 || !words(0).equals(word)) {
            val embedding_out = weights(i)
            val score = TensorUtils.cosineDistance(embedding_in, embedding_out)
            if (pq.size < k) pq.enqueue(word -> score)
            else if (score > pq.head._2) {
              // if the score is greater the min, then add to the heap
              pq.dequeue()
              pq.enqueue(word -> score)
            }
          }
        }
      }
    }
    pq.toSeq
  }

  def stringNearestNeighbors(inString : String, filter :Boolean = false) : Seq[String] = //(String, Double)] =
  {
    val knn = 3
    val terms = 10
    val phrases = WordVectorUtils.extractPhrasesWindow(inString, this)
    val expTerms = phrases.map(t => {
      var nn = nearestNeighbors(Array(t), word2Vec(t), knn)
      if (filter) nn.filter(w => StringUtils.getLevenshteinDistance(w._1, t).toDouble/((t.length + w._1.length)/2) <= .5)
      val q = nn.map(w=>{
        if (w._1.contains('_')) s" #ordered(${w._1.replaceAll("_", " ")}) " else w._1
      })
      s" #synonym( ${q.mkString(" ")} )"
    })
//      .toSeq.sortBy(-_._2).take(terms).map(w =>{
//      if (w._1.contains('_')) (s"#sdm(${w._1.replaceAll("_"," ")})", w._2)
//      else w
//    }).filterNot(w => nlp.lexicon.StopWords.containsWord(w._1))
    expTerms.toSeq
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

  def averageVectors(tensors : Iterable[Tensor]) : DenseTensor1 ={
    val embedding_in = new DenseTensor1(D, 0)
    tensors.foreach(tensor => {
        embedding_in.+=(tensor)
    })
    embedding_in./=(tensors.size)
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
    val underscoreCount = word.length() - word.replace("_", "").length()
    val vocab = if (underscoreCount == 0)
      unigrams
    else if (underscoreCount == 1)
      bigrams
    else if (underscoreCount == 2)
      trigrams
    else // vocab only supports up to trigrams
      return -1
    vocab.getOrElse(word, -1)
  }
}




