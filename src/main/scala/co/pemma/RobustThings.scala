package co.pemma

import java.io.File

import cc.factorie.la.DenseTensor1
import co.pemma.embeddings.{WordVectorMath, WordVectorsSerialManager}
import edu.umass.ciir.strepsi.trec.TrecRunWriter
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryLib, GalagoQueryBuilder, GalagoSearcher}
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

  ////// done initializing ///////

  //  val vectorLocation = "./vectors/newswire-vectors.dat"
  val vectorLocation = "./vectors/serial-vectors"
  val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors(vectorLocation))

  // process the querytext, convert to galgo
  //  val queryId = if (args.length > 0) args(0).toInt else queries.head._1
  //  val queryText = queries.getOrElse(queryId, "")

  for((queryId, queryText) <- queries)
  {
    println(s"Embedding reranking for query $queryId : $queryText")
    val galagoQuery = GalagoQueryBuilder.seqdep(defaultStopStructures.removeStopStructure(queryText)).queryStr

    // get expansion and top docs for robust and wiki
    val (collectionTerms, collectionRankings) = ExpansionModels.expansionTerms(robustSearcher, galagoQuery, 1000, 5, 20)
    val collectionDocs = collectionRankings.map(doc => robustSearcher.pullDocumentWithTokens(doc.documentName))

    // setup embeddings
    val queryPhrases = wordVecs.extractPhrasesWindow(queryText)
    val queryTensor = wordVecs.averageVectors(wordVecs.words2Vectors(queryPhrases))

    baseLineResults(queryId, galagoQuery, collectionRankings, collectionTerms, queryTensor, queryText)
    sumAndClusterDocs(queryId, collectionRankings, collectionDocs, queryTensor)
    bestMatchedSentences(queryId, collectionDocs, queryTensor)

  }

  def baseLineResults(queryId: Int = 0, galagoQuery : String, collectionRankings: Seq[ScoredDocument],
                      collectionTerms : Seq[(String, Double)], queryTensor : DenseTensor1, queryText: String): Unit =
  {
    val (wikiTerms, wikiRankings) = ExpansionModels.expansionTerms(wikiSearcher, galagoQuery, 25, 5, 20, "wikipedia")
    val wikiEntities = wikiRankings.map(_.documentName)
    val embeddingTerms = wordVecs.nearestNeighbors(Array(queryText), queryTensor, 20).toSeq.map(w => (wordVecs.cleanString(w._1),w._2))
    // find top knn embeddings for each rm term and weight it by embedding distance * expansion value
    val rmEmbedExpansionTerms = collectionTerms.flatMap(term => {
      val termTensor = wordVecs.word2Vec(term._1)
      if (termTensor != null) Seq(term) ++ wordVecs.nearestNeighbors(Array(term._1), termTensor, 3).map(n => (wordVecs.cleanString(n._1), n._2*term._2))
      else Seq(term)
    })

    val (wikiExpansionRankings,_) = ExpansionModels.runExpansionQuery(galagoQuery, wikiTerms, "robust", robustSearcher)
    val (rmExpansionRankings,_) = ExpansionModels.runExpansionQuery(galagoQuery, collectionTerms, "robust", robustSearcher)
    val (embeddingExpansionRankings,_) = ExpansionModels.runExpansionQuery(galagoQuery, embeddingTerms, "robust", robustSearcher)
    val (rmEmbedExpansionRankings,_) = ExpansionModels.runExpansionQuery(galagoQuery, rmEmbedExpansionTerms, "robust", robustSearcher)

    // export baseline results
    formatAndExportRankings(queryId, collectionRankings, "sdm")
    formatAndExportRankings(queryId, wikiExpansionRankings, "wiki-expansion")
    formatAndExportRankings(queryId, rmExpansionRankings, "rm-expansion")
    formatAndExportRankings(queryId, embeddingExpansionRankings, "embed-expansion")
    formatAndExportRankings(queryId, rmEmbedExpansionRankings, "rm-embed-expansion")
  }

  def formatAndExportRankings(queryId: Int = 0,  rankings: Seq[ScoredDocument], outputDir : String): Unit ={
    val rankingsFormated = rankings.map(d => (d.documentName, d.score, d.rank))
    TrecRunWriter.writeRunFileFromTuple(new File(s"out/$outputDir/$queryId"), Seq((queryId + "", rankingsFormated)))
  }

  /** given a pool of documents, rank by embedding sum and best cluster match - export results to file
   * @param queryId default = 0
   * @param collectionRankings pool of documents, sdm export based on rankings
   * @param queryVector querytext converted to wordvector
   */
  def sumAndClusterDocs(queryId: Int = 0, collectionRankings: Seq[ScoredDocument], collectionDocs : Seq[org.lemurproject.galago.core.parse.Document], queryVector : DenseTensor1) {
    // grab the actual docs form galago
    val embeddingRankings = collectionDocs.zipWithIndex.map({ case (doc, i) =>
      val docPhrases = wordVecs.extractPhrasesWindow(DocReader.parseRobust(doc.text))
      val docTensors = wordVecs.words2Vectors(docPhrases)

      // get score from summing words in doc
      val avgDocTensor = wordVecs.averageVectors(docTensors)
      val docSumSimilarity = queryVector.cosineSimilarity(avgDocTensor)

      // convert document to centroids
      val docCentroids = Clusterer.documentCentroids(docTensors, wordVecs, 10, 250)
      val bestCentroidSimilarity = docCentroids.map(centroid => queryVector.cosineSimilarity(centroid)).max

      (doc, bestCentroidSimilarity, docSumSimilarity)
    })

    // sort and export rankings
    val centroidRankings = embeddingRankings.sortBy(-_._2).zipWithIndex.map({case(d, i) => (d._1.name, d._2, i+1) })
    val sumRankings = embeddingRankings.sortBy(-_._3).zipWithIndex.map({ case (d, i) => (d._1.name, d._3, i + 1)})
    TrecRunWriter.writeRunFileFromTuple(new File(s"out/centroid/$queryId"), Seq((queryId + "", centroidRankings)))
    TrecRunWriter.writeRunFileFromTuple(new File(s"out/sum/$queryId"), Seq((queryId + "", sumRankings)))
  }

  /*
    score each documents similarity to the query based on the avg of the top k sentences
   */
  def bestMatchedSentences(queryId: Int, documents: Seq[org.lemurproject.galago.core.parse.Document], queryTensor: DenseTensor1)
  {
    val topK = 3

    val sentenceRankings = documents.map(doc =>
    {
      val facDoc = new cc.factorie.app.nlp.Document(doc.text)
      FactorieUtils.segmentPipeline.process(facDoc)
      // get the topK most similar sentences to the query for this doc
      val sentSimilarity = facDoc.sentences.map(s => {
        // convert each sentance to an averaged vector
        val sentencePhrases = wordVecs.extractPhrasesWindow(s.string)
        val sentenceTensors = wordVecs.words2Vectors(sentencePhrases)
        val sim = queryTensor.cosineSimilarity(wordVecs.averageVectors(sentenceTensors))
        (sim, s)
      }).toSeq.sortBy(-_._1) take topK

//      println("\n\n\n" + doc.text + "\n")
//      sentSimilarity.foreach(println(_.string))

      (doc, sentSimilarity.map(_._1).sum / topK )
      // sort results, put in correct form for output
    }).sortBy(-_._2).zipWithIndex.map({ case (d, i) => (d._1.name, d._2, i + 1)})

    // export rankings in trec format
    TrecRunWriter.writeRunFileFromTuple(new File(s"out/sentence/$queryId"), Seq((queryId + "", sentenceRankings)))
  }

}