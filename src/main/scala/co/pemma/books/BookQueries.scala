package co.pemma.books

import edu.umass.ciir.ede.features.ExpansionModels
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import org.lemurproject.galago.utility.Parameters

/**
 * Created by pv on 11/19/14.
 */
class BookQueries extends App{

  // set things up
  val bookIndex = "./index/books-index_small"
  val searcher: GalagoSearcher = {
    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    GalagoSearcher(indexParam)
  }
  val defaultStopStructures = new StopStructuring(searcher.getUnderlyingRetrieval())
  val numDocs = 1000
  val query = "indians north america"
  val galagoQuery = GalagoQueryBuilder.seqdep(defaultStopStructures.removeStopStructure(query)).queryStr

  // run the query and pull the top results
  val params = ExpansionModels.getCollectionParams("books")
  val rankings = searcher.retrieveScoredDocuments(galagoQuery, Some(params), numDocs)
  val collectionDocs = rankings.map(doc => searcher.pullDocumentWithTokensAndMeta(doc.documentName))


}
