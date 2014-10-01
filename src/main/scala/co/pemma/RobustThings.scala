package co.pemma

import cc.factorie.app.nlp.lexicon.StopWords
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap}
import cc.factorie.la.DenseTensor1
import co.pemma.embeddings.{WordVectorUtils, WordVectorMath, WordVectorsSerialManager}
import edu.umass.ciir.ede.features.ExpansionModels
import edu.umass.ciir.strepsi.trec.TrecRunWriter
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import main.scala.co.pemma.Clusterer
import org.lemurproject.galago.core.parse
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

  ////// done initializing ///////

  val vectorLocation = "./vectors/serial-vectors"
  val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserialize(vectorLocation))

  // process the querytext, convert to galgo
  //  val queryId = if (args.length > 0) args(0).toInt else queries.head._1
  //  val queryText = queries.getOrElse(queryId, "")

  for((queryId, queryText) <- queries)
  {
    println(s"Embedding reranking for query $queryId : $queryText")
    val galagoQuery = GalagoQueryBuilder.seqdep(defaultStopStructures.removeStopStructure(queryText)).queryStr

    // get expansion and top docs for robust and wiki
    val (collectionTerms, collectionRankings) = ExpansionModels.expansionTerms(robustSearcher, galagoQuery, 1000, 5, 20)
    val (wikiTerms, wikiRankings) = ExpansionModels.expansionTerms(wikiSearcher, galagoQuery, 25, 5, 20, "wikipedia")
    val wikiEntities = wikiRankings.map(_.documentName)
    val collectionDocs = collectionRankings.map(doc => robustSearcher.pullDocumentWithTokens(doc.documentName))

    // setup embeddings
    //  val vectorLocation = "./vectors/newswire-vectors.dat"
    val queryTensor = wordVecs.averageVectors(WordVectorUtils.words2Vectors(WordVectorUtils.extractPhrasesWindow(queryText, wordVecs), wordVecs)) //wordVecs.phrase2Vec(queryText)

//    sumAndClusterDocs(queryId, collectionRankings, collectionDocs, queryTensor)

    bestMatchedSentences(queryId, collectionDocs, queryTensor)
  }

  /** given a pool of documents, rank by embedding sum and best cluster match - export results to file
   * @param queryId default = 0
   * @param collectionRankings pool of documents, sdm export based on rankings
   * @param queryVector querytext converted to wordvector
   */
  def sumAndClusterDocs(queryId: Int = 0, collectionRankings: Seq[ScoredDocument], collectionDocs : Seq[org.lemurproject.galago.core.parse.Document], queryVector : DenseTensor1) {
    // grab the actual docs form galago
    val embeddingRankings = collectionDocs.zipWithIndex.map({ case (doc, i) =>
      //    val docStringArray: Iterable[String] = WordVectorUtils.extractPhrasesFactorie(DocReader.parseRobust(doc.text))
      val docStringArray: Iterable[String] = WordVectorUtils.extractPhrasesWindow(DocReader.parseRobust(doc.text), wordVecs)
      // convert phrases/words to tensors
      val docTensors = WordVectorUtils.words2Vectors(docStringArray, wordVecs)
      // convert document to centroids
      //      val docCentroids = Clusterer.documentCentroids(docTensors, wordVecs, 10, 250)
      val bestCentroidDistance = 0 //docCentroids.map(centroid => queryVector.cosineSimilarity(centroid)).max
    val sumDistance = queryVector.cosineSimilarity(wordVecs.averageVectors(docTensors))

      (doc, bestCentroidDistance, sumDistance)
    })

    // sort and export rankings
    //    val centroidRankings = embeddingRankings.sortBy(-_._2).zipWithIndex.map({case(d, i) => (d._1.name, d._2, i+1) })
    val sumRankings = embeddingRankings.sortBy(-_._3).zipWithIndex.map({ case (d, i) => (d._1.name, d._3, i + 1)})
    val sdmRankings = collectionRankings.map(d => (d.documentName, d.score, d.rank))

//    TrecRunWriter.writeRunFileFromTuple(new File(s"out/centroid-$queryId"), Seq((queryId + "", centroidRankings)))
    TrecRunWriter.writeRunFileFromTuple(new File(s"out/sum-$queryId"), Seq((queryId + "", sumRankings)))
    TrecRunWriter.writeRunFileFromTuple(new File(s"out/sdm-$queryId"), Seq((queryId + "", sdmRankings)))
  }

  /*
    score each documents similarity to the query based on the avg of the top k sentences
   */
  def bestMatchedSentences(queryId: Int, documents: Seq[org.lemurproject.galago.core.parse.Document], queryTensor: DenseTensor1)
  {
    val topK = 10

    val sentenceRankings = documents.map(doc =>
    {
      val facDoc = new cc.factorie.app.nlp.Document(doc.text)
      FactorieUtils.segmentPipeline.process(facDoc)
      // get the topK most similar sentences to the query for this doc
      val sentSimilarity = facDoc.sentences.map(s => {
        // convert each sentance to an averaged vector
        val sentencePhrases = WordVectorUtils.extractPhrasesWindow(s.string, wordVecs)
        val sentenceTensors = WordVectorUtils.words2Vectors(sentencePhrases, wordVecs)
        val sim = queryTensor.cosineSimilarity(wordVecs.averageVectors(sentenceTensors))
        (sim, s)
      }).toSeq.sortBy(-_._1) take topK

//      println("\n\n\n" + doc.text + "\n")
//      sentSimilarity.foreach(println(_.string))

      (doc, sentSimilarity.map(_._1).sum / topK )
      // sort results, put in correct form for output
    }).sortBy(_._2).zipWithIndex.map({ case (d, i) => (d._1.name, d._2, i + 1)})

    // export rankings in trec format
    TrecRunWriter.writeRunFileFromTuple(new File(s"out/sentence-$queryId"), Seq((queryId + "", sentenceRankings)))
  }

}