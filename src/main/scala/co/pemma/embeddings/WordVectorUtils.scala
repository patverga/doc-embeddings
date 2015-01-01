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

class WordVectorUtils(embedding : WordVectors){
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

  def editDistanceNearestNeighbors(inString : String, filter :Boolean = false, usePhrases:Boolean = true, threshold :Double = 0.33, knn : Int = 10) : Seq[String] = //(String, Double)] =
  {
    val tokens : Iterable[String] = if (usePhrases) extractPhrasesWindow(inString) else inString.split("\\s+")
    val expTerms = tokens.map(t => {
      var nn = nearestNeighbors(Array(t), word2Vec(t), knn)
      if (filter) nn = nn.filter(w => StringUtils.getLevenshteinDistance(w._1, t).toDouble/((t.length + w._1.length)/2.0) <= threshold)
      val q = nn.map(w=>{
        if (w._1.contains('_')) s" #ordered(${w._1.replaceAll("_", " ")}) " else w._1
      })
      s" #synonym( ${q.mkString(" ")} )"
    })
    expTerms.toSeq
  }

  def queryNearestNeighbors(inString : String, filter :Boolean = false, usePhrases:Boolean = true, knn:Int = 10) : Seq[(String, Double)] =
  {
    val tokens : Iterable[String] = if (usePhrases) extractPhrasesWindow(inString) else inString.split("\\s+")
    val expTerms = tokens.flatMap(t => {
      nearestNeighbors(Array(t), word2Vec(t), knn).map(w=>{
        (if (w._1.contains('_')) w._1.replaceAll("_", " ") else w._1, w._2)
      }) ++ Seq((t, .8))
    })
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

  /**
   * Look over sliding window of 4 words at a time to extract trigram bigram and unigram phrases
   * contained in our vector model
   * @param doc preprocessed document
   * @return words and phrases from doc with vector representations
   */
  def extractPhrasesWindow(doc: String): Iterable[String] =
  {
    val wordArray = doc.split("\\s+")
    val phraseList = new mutable.ListBuffer[String]()

    var i = 0
    while (i < wordArray.length)
    {
      // check trigram 1-2-3
      val triResult = if (i==0 && i+2 < wordArray.length){
        val trigram = s"${wordArray(i)}_${wordArray(i+1)}_${wordArray(i+2)}"
        checkPhraseVectorExists(trigram, trigrams) } else null
      if (triResult != null) {phraseList += triResult; i+=3 }
      else
      {
        // check trigram 2-3-4
        val tirResult2 = if (i+3 < wordArray.length){
          val trigram2 = s"${wordArray(i+1)}_${wordArray(i+2)}_${wordArray(i+3)}"
          checkPhraseVectorExists(trigram2, trigrams)
        } else null
        if (tirResult2 != null){ phraseList += tirResult2; phraseList += wordArray(i); i+=4 }
        // check bigram 1-2
        else {
          // check trigram 2-3-4
          val biResult = if (i+1 < wordArray.length){
            val bigram = s"${wordArray(i)}_${wordArray(i+1)}"
            checkPhraseVectorExists(bigram, bigrams)
          } else null
          if (biResult != null){ phraseList += biResult; i+=2 }
          else { phraseList += wordArray(i); i+=1 }
        }
      }
    }
    //    println(doc + "\n\n")
    //    phraseList.foreach(p => print( p + " "))
    phraseList
  }

  def checkPhraseVectorExists(phrase: String, wordVectors :  mutable.Map[String, Int]) : String = {
    // try
    if (wordVectors.contains(phrase))
      phrase
    else {
      val cap = WordUtils.capitalizeFully(phrase, '_')
      if (wordVectors.contains(cap))
        cap
      else {
        val lower = phrase.toLowerCase
        if (wordVectors.contains(lower))
          lower
        else null
      }
    }
  }

  def words2Vectors(doc: Iterable[String], keepStopWords : Boolean = false):
  //  Iterable[(String, DenseTensor1)] = {
  Iterable[DenseTensor1] = {
    // filterStopWords
    val words = if (keepStopWords) doc else doc.filter(w => !nlp.lexicon.StopWords.containsWord(w) && w.size > 1)
    // turn each word into an embedding vector
    for (w <- words; wv = word2Vec(w); if wv != null)
    //    yield (w, wv)
    yield wv
  }

  def cleanString(inputStr : String): String =
  {
    inputStr.replaceAll("_", " ").replaceAll("\\p{P}|#", "")
  }
}




