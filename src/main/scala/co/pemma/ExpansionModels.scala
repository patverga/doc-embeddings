package edu.umass.ciir.ede.features

import edu.umass.ciir.strepsi.ciirshared.LanguageModel
import edu.umass.ciir.strepsi.galagocompat.GalagoTag
import edu.umass.ciir.strepsi.{LogTools, StopWordList, TextNormalizer}
import edu.umass.ciir.strepsimur.galago.compat.CompatConverters._
import edu.umass.ciir.strepsimur.galago.{GalagoQueryLib, GalagoSearcher, DocumentPullException, DocumentPuller}
import org.lemurproject.galago.core.parse.Document
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.utility.Parameters

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * Created by jdalton on 1/19/14.
 */

case class FeedbackParams(docProbWeight: Double,
                          docProb: Double,
                          wikiCfWeight: Double,
                          rob04CfWeight: Double,
                          numFeedbackDocs: Int,
                          numFeedbackTerms: Int,
                          lambda: Double)

object ExpansionModels {
  val feedbackParams = FeedbackParams(1.0, 0, 0, 0, 25, 20,  0.55)

  def lce(baseRanking: Seq[ScoredDocument],
          docPuller: DocumentPuller[Document],
          numTerms: Int, collection : String = "robust"): Seq[(String, Double)] = {

    val docProbs = LogTools.logExpSumNormalize[ScoredDocument](baseRanking, _.score).map(
      e => e._1.documentName -> e._2).toMap

    val weightMap = scala.collection.mutable.Map[String, Double]().withDefaultValue(0.0d)

    val p = new Parameters()
    p.set("text", true)
    p.set("tokenize", true)
    p.set("metadata", true)

    for (doc <- baseRanking)
    {
      val docId = doc.documentName

      if (docId.size > 0) {
        try{
          val entityDoc = docPuller.pullDocument(docId, p)
          val docTerms = if (collection equals "wikipedia") {
            val fieldTokens = fieldTokenMap(entityDoc.terms, doc2Tag(entityDoc))
            fieldTokens.map(_._2).flatten.toSeq
          }
          else
            entityDoc.terms.toSeq

          val fieldLm = new LanguageModel(1)
          fieldLm.addDocument(docTerms, false)
          fieldLm.calculateProbabilities()

          val scoreNorm = feedbackParams.docProbWeight + feedbackParams.docProb + feedbackParams.wikiCfWeight + feedbackParams.rob04CfWeight

          for (te <- fieldLm.getEntries) {
            val wikiProb = 1
            val robustProb = 1
            val docProb = (feedbackParams.docProbWeight / scoreNorm) * Math.log(docProbs(doc.documentName) * te.getProbability)
            val termProb = (feedbackParams.docProb / scoreNorm) * Math.log(te.getProbability)
            val wikiComponent = (feedbackParams.wikiCfWeight / scoreNorm) * Math.log(wikiProb)
            val collectionComponent = (feedbackParams.rob04CfWeight / scoreNorm) * Math.log(robustProb)
            val combined = docProb + termProb - wikiComponent - collectionComponent
            //weightMap.update(te.getTerm, weightMap(te.getTerm) + (1.0 / baseRanking.size * Math.exp(combined)))
            weightMap.update(te.getTerm, weightMap(te.getTerm) + Math.exp(
              combined)) //  old bug here!  This did not normalize by the number of documents.

          }
        }
        catch{
          case ex:DocumentPullException => {   }
        }
      }


    }

    val topTerms = extractTopTerms(weightMap.toList, numTerms)

    topTerms

  }
  def extractTopTerms(terms: Seq[(String, Double)], numTerms: Int) = {
    StopWordList.removeStopWord("us")
    val topTerms =
      terms.toList.sortBy(-_._2)
        .filterNot(t => {
        StopWordList.isStopWord(t._1) ||
          TextNormalizer.normalizeText(t._1).trim.length < 2 ||
          """\d+""".r == t
      })
        .take(numTerms)

    topTerms
  }

  def fieldTokenMap(terms:Seq[String], tags:Seq[GalagoTag]) = {
    val fieldsToCount = Set("anchor", "title", "redirect", "fbname", "text")
    val tokenMap = scala.collection.mutable.HashMap[String, ListBuffer[String]]()

    for (reqField <- fieldsToCount) {
      tokenMap.put(reqField, ListBuffer[String]())
    }

    for (f <- tags) {
      if (fieldsToCount contains f.name) {
        val fieldTokens = terms.subList(f.begin, f.end)

        val curTokenList = tokenMap.getOrElseUpdate(f.name, ListBuffer[String]()) ++= fieldTokens
      }
    }
    //   tokenMap.put("all", new ListBuffer()++=terms)
    tokenMap.toMap
  }

  def expansionTerms(galagoSearcher: GalagoSearcher, galagoQuery: String, numDocs: Int = 1000, expansionDocs : Int = 5, numTerms: Int = 20, collection: String = "robust")
  : (Seq[(String, Double)], Seq[ScoredDocument]) = {
    val params = getCollectionParams(collection)
    val rankings = galagoSearcher.retrieveScoredDocuments(galagoQuery, Some(params), numDocs)
    (ExpansionModels.lce(rankings take expansionDocs, galagoSearcher, numTerms, collection), rankings)
  }

  def runExpansionQuery(galagoQuery : String, expansionTerms :Seq[(String, Double)], collection:String, searcher : GalagoSearcher, numResults : Int = 1000)
  : Seq[ScoredDocument] = {
    val params = getCollectionParams(collection)
    val expandedQuery = GalagoQueryLib.buildWeightedCombine(Seq((galagoQuery, 0.55), (GalagoQueryLib.buildWeightedCombine(expansionTerms take 20), 1 - 0.55)))
    println("running query: " + expandedQuery)
    searcher.retrieveScoredDocuments(expandedQuery, Some(params), numResults)
  }

  def getCollectionParams(collection : String) : Parameters ={
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
    p
    }
}
