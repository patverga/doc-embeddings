package co.pemma

import cc.factorie.app.nlp.lexicon.StopWords
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.{Document, DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap}
import co.pemma.embeddings.{WordVectorMath, WordVectorsSerialManager}
import edu.umass.ciir.ede.features.ExpansionModels
import edu.umass.ciir.strepsi.trec.TrecRunWriter
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import main.scala.co.pemma.Clusterer
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.core.tokenize.Tokenizer
import org.lemurproject.galago.utility.Parameters

import java.io.File
import scala.collection.JavaConversions._

/**
 * Created by pat on 9/26/14.
 */
object RobustThings extends App {
  ////// initializing things ///////
  val robustIndexLocation = "./index/robust04-g35"
  val queries = loadTsvQueries("./data/rob04.titles.tsv")
  val robustSearcher: GalagoSearcher = {
    val indexParam = new Parameters()
    indexParam.set("index", robustIndexLocation)
    GalagoSearcher(indexParam)
  }
  val wikiSearcher = GalagoSearcher("./index/wikipedia")
  val tokenizeText = Tokenizer.instance(new Parameters).tokenize(_: String).terms.toSeq
  val defaultStopStructures = new StopStructuring(robustSearcher.getUnderlyingRetrieval())

  def loadTsvQueries(queryFilename: String, prefix: String = ""): Map[Int, String] = {
    val source = scala.io.Source.fromFile(queryFilename)
    val lines = source.getLines().toList
    val queries =
      for (line <- lines) yield {
        val qInfo = line.split("\t")
        val queryId = qInfo(0)
        val queryString = qInfo(1)

        queryId.replace(prefix, "").toInt -> queryString

      }
    source.close()
    queries.toMap
  }

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
  val pipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, nlpSteps.flatMap(_.postAttrs))

  ////// done initializing ///////


  // process the querytext, convert to galgo
  val queryId = if (args.length > 0) args(0).toInt else queries.head._1
  val queryText = queries.getOrElse(queryId, "")
  println(s"Embedding reranking for query $queryId : $queryText")
  val galagoQuery = GalagoQueryBuilder.seqdep(defaultStopStructures.removeStopStructure(queryText)).queryStr

  // get expansion and top docs for robust and wiki
  val (collectionTerms, collectionRankings) = ExpansionModels.expansionTerms(robustSearcher, galagoQuery, 1000, 5, 20)
  val (wikiTerms, wikiRankings) = ExpansionModels.expansionTerms(wikiSearcher, galagoQuery, 25, 5, 20, "wikipedia")
  val wikiEntities = wikiRankings.map(_.documentName)

  // setup embeddings
//  val vectorLocation = "./vectors/newswire-vectors.dat"
  val vectorLocation = "./vectors/serial-vectors"
  val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserialize(vectorLocation))
  val queryVector = wordVecs.phrase2Vec(queryText)

  // grab the actual docs form galago
  val collectionDocs = collectionRankings.map(doc => robustSearcher.pullDocumentWithTokens(doc.documentName))
  val embeddingRankings = collectionDocs.map(doc =>
  {
    // use factorie for phrase chunking
    val usedTokens = scala.collection.mutable.Set[Int]()
    val facDoc = pipeline.process(new Document(DocReader.parseRobust(doc.text)))
    val docString = for (phrase <- facDoc.attr[PhraseList] if !StopWords.containsWord(phrase.string))
    yield {
      phrase.tokens.foreach(usedTokens += _.position)
      phrase.string
    }

    // collect phrases and tokens not in phrases
    val docStringArray = docString ++ (for (token <- facDoc.tokens; str = token.string
                              if !usedTokens.contains(token.position) &&
                                !StopWords.containsWord(str.toLowerCase) &&
                                str.size > 1) yield token.string)

    // convert document to centroids
//    val docCentroids = Clusterer.documentCentroids(docStringArray, wordVecs, 10, 250)
    val bestCentroidDistance = 0 //docCentroids.map(centroid => queryVector.cosineSimilarity(centroid)).max
    val sumDistance = queryVector.cosineSimilarity(wordVecs.sumPhrases(docStringArray))

    (doc.name, bestCentroidDistance, sumDistance)
  })

  // sort and export rankings
//  val centroidRankings = embeddingRankings.sortBy(-_._2).zipWithIndex.map({case(d, i) => (d._1.name, d._2, i+1) })
  val sumRankings = embeddingRankings.sortBy(-_._3).zipWithIndex.map({case(d, i) => (d._1, d._3, i+1) })
//  TrecRunWriter.writeRunFileFromTuple(new File(s"out/centroid-$queryId"), Seq((queryId+"", centroidRankings)))
  TrecRunWriter.writeRunFileFromTuple(new File(s"out/sum-$queryId"), Seq((queryId+"", sumRankings)))

}