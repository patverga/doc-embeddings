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

  // initialize pipeline
//  val nlpSteps = Seq(
//    OntonotesForwardPosTagger,
//    NoEmbeddingsConllStackedChainNer,
//    OntonotesTransitionBasedParser,
//    BILOUChainChunker,
//    NPChunkMentionFinder
//    //    NounPhraseEntityTypeLabeler
//  )
//  val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
//  for (annotator <- nlpSteps) map += annotator
//  val pipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, nlpSteps.flatMap(_.postAttrs))

  //// DONE INITIALIZING /////

//  def extractPhrasesFactorie(doc: parse.Document): IndexedSeq[String] = {
//    // use factorie for phrase chunking
//    val usedTokens = scala.collection.mutable.Set[Int]()
//    val facDoc = pipeline.process(new Document(DocReader.parseRobust(doc.text)))
//    val docString = for (phrase <- facDoc.attr[PhraseList] if !StopWords.containsWord(phrase.string))
//    yield {
//      phrase.tokens.foreach(usedTokens += _.position)
//      phrase.string
//    }
//
//    // collect phrases and tokens not in phrases
//    docString ++ (for (token <- facDoc.tokens; str = token.string
//                       if !usedTokens.contains(token.position) &&
//                         !StopWords.containsWord(str.toLowerCase) &&
//                         str.size > 1) yield token.string)
//  }

  def extractPhrasesWindow(doc: String, wordVectors : WordVectorMath): Iterable[String] =
  {
    val wordArray = doc.split("\\s+")
    val phraseList = new mutable.ListBuffer[String]()

    var i = 0

    // looking over window length 4
    while (i+4 < wordArray.length)
    {
      // check trigram 1-2-3
      val tri = if (i==0){
        val trigram = s"${wordArray(i)}_${wordArray(i+1)}_${wordArray(i+2)}"
        checkPhraseVectorExists(trigram, wordVectors.trigrams) } else null
      if (tri != null) {phraseList += tri; i+=3 }
      else 
      {
        // check trigram 2-3-4
        val trigram2 = s"${wordArray(i+1)}_${wordArray(i+2)}_${wordArray(i+3)}"
        val tri2 = checkPhraseVectorExists(trigram2, wordVectors.trigrams)
        if (tri2 != null){ phraseList += tri2; phraseList += wordArray(i); i+=4 }
          // check bigram 1-2
        else {
          // check trigram 2-3-4
          val bigram = s"${wordArray(i)}_${wordArray(i+1)}"
          val bi = checkPhraseVectorExists(bigram, wordVectors.bigrams)
          if (tri2 != null){ phraseList += bi; i+=1 }
          else phraseList += wordArray(i); i+= 1
        }
      }
    }
    phraseList
  }

  def checkPhraseVectorExists(trigram: String, wordVectors :  mutable.Map[String, Int]) : String = {
    if (wordVectors.contains(trigram))
      trigram
    else if (wordVectors.contains(WordUtils.capitalizeFully(trigram, '_')))
      WordUtils.capitalizeFully(trigram, '_')
    else if (wordVectors.contains(trigram.toLowerCase))
      trigram.toLowerCase
    else null
  }

  def words2Vectors(doc: Iterable[String], wordVectors: WordVectorMath, keepStopWords : Boolean = false):
  Iterable[(String, DenseTensor1)] = {
    // filterStopWords
    val words = if (keepStopWords) doc else doc.filter(w => !nlp.lexicon.StopWords.containsWord(w) && w.size > 1)
    // turn each word into an embedding vector
    for (w <- words; wv = wordVectors.word2Vec(w); if wv != null)
    yield (w, wv)
  }
}
