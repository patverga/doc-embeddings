package co.pemma

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.lexicon.StopWords
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase.{BILOUChainChunker, NPChunkMentionFinder, PhraseList}
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger

/**
 * Created by pv on 9/30/14.
 */
object FactorieUtils
{
  val segmentPipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))
  var phrasePipeline = DocumentAnnotatorPipeline()

  def initializePhrasePipeline(): Unit =
  {
    val nlpSteps = Seq(
      OntonotesForwardPosTagger,
      NoEmbeddingsConllStackedChainNer,
      OntonotesTransitionBasedParser,
      BILOUChainChunker,
      NPChunkMentionFinder
      //    NounPhraseEntityTypeLabeler
    )
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    for (annotator <- nlpSteps) map += annotator
    phrasePipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, nlpSteps.flatMap(_.postAttrs))
  }

  def extractPhrasesFactorie(doc: String): IndexedSeq[String] =
  {
    if (phrasePipeline == null) initializePhrasePipeline()
    // use factorie for phrase chunking
    val usedTokens = scala.collection.mutable.Set[Int]()
    val facDoc = phrasePipeline.process(new Document(doc))
    val docString = for (phrase <- facDoc.attr[PhraseList] if !StopWords.containsWord(phrase.string))
    yield {
      phrase.tokens.foreach(usedTokens += _.position)
      phrase.string
    }

    // collect phrases and tokens not in phrases
    docString ++ (for (token <- facDoc.tokens; str = token.string
                       if !usedTokens.contains(token.position) &&
                         !StopWords.containsWord(str.toLowerCase) &&
                         str.size > 1) yield token.string)
  }
}
