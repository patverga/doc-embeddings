package co.pemma.embeddings

import cc.factorie.app.nlp
import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap, Document}
import cc.factorie.app.nlp.lexicon.StopWords
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase.{NPChunkMentionFinder, BILOUChainChunker, PhraseList}
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.la.DenseTensor1
import co.pemma.DocReader
import org.apache.commons.lang3.text.WordUtils
import org.lemurproject.galago.core.parse
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by pat on 9/30/14.
 */
object WordVectorUtils {

  /**
   * Look over sliding window of 4 words at a time to extract trigram bigram and unigram phrases
   * contained in our vector model
   * @param doc preprocessed document
   * @param wordVectors word vector model to check contains with
   * @return words and phrases from doc with vector representations
   */
  def extractPhrasesWindow(doc: String, wordVectors : WordVectorMath): Iterable[String] =
  {
    val wordArray = doc.split("\\s+")
    val phraseList = new mutable.ListBuffer[String]()

    var i = 0
    while (i < wordArray.length)
    {
      // check trigram 1-2-3
      val triResult = if (i==0 && i+2 < wordArray.length){
        val trigram = s"${wordArray(i)}_${wordArray(i+1)}_${wordArray(i+2)}"
        checkPhraseVectorExists(trigram, wordVectors.trigrams) } else null
      if (triResult != null) {phraseList += triResult; i+=3 }
      else 
      {
        // check trigram 2-3-4
        val tirResult2 = if (i+3 < wordArray.length){
          val trigram2 = s"${wordArray(i+1)}_${wordArray(i+2)}_${wordArray(i+3)}"
          checkPhraseVectorExists(trigram2, wordVectors.trigrams)
        } else null
        if (tirResult2 != null){ phraseList += tirResult2; phraseList += wordArray(i); i+=4 }
          // check bigram 1-2
        else {
          // check trigram 2-3-4
          val biResult = if (i+1 < wordArray.length){
            val bigram = s"${wordArray(i)}_${wordArray(i+1)}"
            checkPhraseVectorExists(bigram, wordVectors.bigrams)
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

  def words2Vectors(doc: Iterable[String], wordVectors: WordVectorMath, keepStopWords : Boolean = false):
//  Iterable[(String, DenseTensor1)] = {
  Iterable[DenseTensor1] = {
    // filterStopWords
    val words = if (keepStopWords) doc else doc.filter(w => !nlp.lexicon.StopWords.containsWord(w) && w.size > 1)
    // turn each word into an embedding vector
    for (w <- words; wv = wordVectors.word2Vec(w); if wv != null)
//    yield (w, wv)
    yield wv
  }

  def cleanString(inputStr : String): String =
  {
    inputStr.replaceAll("_", " ").replaceAll("\\p{P}|#", "")
  }
}


object TestDistance extends App{
  val inLocation = args(0)
  val outLocation = inLocation+".dat"

  WordVectorsSerialManager.vectorTxt2Serial(inLocation, outLocation)
  val distance = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors(outLocation))
  //  println(distance.phrase2Vec("bill clinton"))
  distance.interactiveNearestNeighbor()
}
