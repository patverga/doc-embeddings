package co.pemma

import cc.factorie.app.nlp
import cc.factorie.app.nlp.lexicon.StopWords
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase.{NounPhraseEntityTypeLabeler, PosBasedNounPhraseFinder}
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.{Token, DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap, Document}
import co.pemma.embeddings.{WordVectorMath, WordVectorsSerialManager}
import edu.umass.ciir.ede.features.ExpansionModels
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import main.scala.co.pemma.Clusterer
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.core.tokenize.Tokenizer
import org.lemurproject.galago.utility.Parameters

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
    PosBasedNounPhraseFinder,
    NounPhraseEntityTypeLabeler
  )
  val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  for (annotator <- nlpSteps) map += annotator
  val pipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, nlpSteps.flatMap(_.postAttrs))

  ////// done initializing ///////


  def expansionTerms(galagoSearcher: GalagoSearcher, query: String, numDocs: Int = 1000, numTerms: Int = 20, collection: String = "robust")
  : (Seq[(String, Double)], Seq[ScoredDocument]) = {
    val params = {
      val p = new Parameters()
      if (collection == "robust") {
        p.set("mu", 1269.0)
        p.set("defaultSmoothingMu", 1269.0)
        p.set("uniw", 0.87264)
        p.set("odw", 0.07906)
        p.set("uww", 0.04829)
        p.set("deltaReady", true)
        p
      }
      else {
        p.set("mu", 96400.0)
        p.set("defaultSmoothingMu", 96400.0)
        p.set("uniw", 0.85)
        p.set("odw", 0.1)
        p.set("uww", 0.05)
        p.set("deltaReady", true)
        p
      }
    }
    val rankings = galagoSearcher.retrieveScoredDocuments(galagoQuery, Some(params), numDocs)
    (ExpansionModels.lce(rankings, galagoSearcher, numTerms, collection), rankings)
  }



  // process the querytext, convert to galgo
  val queryText = queries.head._2
  val galagoQuery = GalagoQueryBuilder.seqdep(defaultStopStructures.removeStopStructure(queryText)).queryStr

  // get expansion and top docs for robust and wiki
  val (collectionTerms, collectionRankings) = expansionTerms(robustSearcher, galagoQuery, 5, 20)
  val (wikiTerms, wikiRankings) = expansionTerms(wikiSearcher, galagoQuery, 5, 20, "wikipedia")
  val wikiEntities = wikiRankings.map(_.documentName)

  // setup embeddings
  val serialLocation = "./vectors/serial-vectors"
//  val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserialize(serialLocation))
//  val queryVector = wordVecs.phrase2Vec(queryText)

  // grab the actual docs form galago
  val collectionDocs = collectionRankings.map(doc => robustSearcher.pullDocumentWithTokens(doc.documentName))
  collectionDocs.foreach(doc => {
    val facDoc = pipeline.process(new Document(DocReader.readRobust(doc.text)))

//    // convert document to centroids
//    val docCentroids = Clusterer.documentCentroids(docString, wordVecs, 35, 250)
//    val bestCentroidDistance = docCentroids.map(centroid => queryVector.cosineSimilarity(centroid)).max
//    val sumDistance = queryVector.cosineSimilarity(wordVecs.sumWords(docString))

//    println(bestCentroidDistance, sumDistance)
  })
}