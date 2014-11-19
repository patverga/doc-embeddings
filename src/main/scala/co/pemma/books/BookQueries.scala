package co.pemma.books

import edu.umass.ciir.ede.features.ExpansionModels
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import org.lemurproject.galago.utility.Parameters
import scala.collection.JavaConverters._
import scala.io.Source

/**
 * Created by pv on 11/19/14.
 */
object BookQueries
{

  def main(args: Array[String])
  {
    // set some params
    val numDocs = 1000
    val output = "./books/output/"

    assert(args.size > 0, " Must supply a query id number.")
    val qid = Integer.parseInt(args(0))

    // read in queries
//    val source = Source.fromURL(getClass.getResource("/book_queries"))
    val source = Source.fromFile("./book_queries")
    val queries = source.getLines().toList
    source.close()
    val query = queries(qid)

    // initialize things
//      val bookIndex = List("./index/books-index_small", "./index/wikipedia").asJava
    val bookIndex = (for (i <- 0 to 20; num = if (i < 10) s"0$i"; else s"$i")
    yield s"/work2/manmatha/michaelz/galago/Proteus/Proteus/homer/mzShards/books-index_$num").toList.asJava

    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    val searcher = GalagoSearcher(indexParam)
    val defaultStopStructures = new StopStructuring(searcher.getUnderlyingRetrieval())

    println(s" Running query number $qid: $query")
    val galagoQuery = GalagoQueryBuilder.seqdep(defaultStopStructures.removeStopStructure(query)).queryStr

    // run the query and pull the top results
    val params = ExpansionModels.getCollectionParams("books")
    val rankings = searcher.retrieveScoredDocuments(galagoQuery, Some(params), numDocs)
    val collectionDocs = rankings.map(doc => (doc.rank, doc.score, searcher.pullDocumentWithTokensAndMeta(doc.documentName)))

    // write results to file
    val p = new java.io.PrintWriter(s"$output/$qid")
    try {
      collectionDocs.foreach { case (rank, score, doc) =>
        p.write(s"$qid \t $rank \t $score \t ${doc.name} \t ${doc.metadata.get("subject")} \t ${doc.metadata.get("year")} \t ${doc.metadata.get("language")} \n")
      }
    }
    finally {
      p.close()
    }
  }
}