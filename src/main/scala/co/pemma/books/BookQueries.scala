package co.pemma.books

import java.io.File

import co.pemma.ExpansionModels
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.tupleflow.Parameters
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by pv on 11/19/14.
 */
object BookQueries
{
  // set some params
  val numDocs = 50000
  val numResults = 1000
  val numExpansionDocs = 10
  val numExpansionTerms = 50
  val output = "./books/output/"
  val langRegex = "[eE]ng(?:lish)?".r
  val yearRegex = "[12][0-9]{3}".r
  val minDate = 1780
  val maxDate = 1930

  def main(args: Array[String])
  {
    assert(args.size > 0, " Must supply a query id number.")
    val qid = Integer.parseInt(args(0))
    val test = if (args.size > 1 && args(1) == "test") true else false

    // read in queries
    val querySource = Source.fromURL(getClass.getResource("/book_queries"))(io.Codec("UTF-8"))
    val queries = querySource.getLines().toList
    querySource.close()
    val query =  queries(qid)

    // read in subjectmap
    val subjectSource = Source.fromURL(getClass.getResource("/subject-id-map"))(io.Codec("UTF-8"))
    val subjects = subjectSource.getLines().map(line => { val parts = line.split("\\|"); parts(0) -> parts(1) }).toMap
    subjectSource.close()

    // make sure this subject is mappable
    assert(subjects.contains(query), s"The query \'$query\' does not exist in the subjects map.")
    println(s" Running query number $qid: $query")

    // set up galago
    val bookIndex = if (test) List("./index/pages-filtered_02").asJava
      else{ (for (i <- 0 to 20; if i != 14; num = if (i < 10) s"0$i"; else s"$i")
        yield s"/work2/manmatha/michaelz/galago/Proteus/Proteus/homer/mzShards/pages-index_$num").toList.asJava
    }
    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    val searcher = GalagoSearcher(indexParam)
    val defaultStopStructures = new StopStructuring(searcher.getUnderlyingRetrieval())

    // run sdm and rm queries and export results
    println("Running SDM Query...")
    val galagoQuery = GalagoQueryBuilder.seqdep(defaultStopStructures.removeStopStructure(query)).queryStr
    val sdmRankings = searcher.retrieveScoredDocuments(galagoQuery, None, numDocs)
    exportResults(qid, query, subjects, "sdm", searcher, sdmRankings)

    println("Running RM Query...")
    val expansionTerms = ExpansionModels.lce(sdmRankings take numExpansionDocs, searcher, numExpansionTerms)
    val rmRankings = ExpansionModels.runExpansionQuery(galagoQuery, expansionTerms, "robust", searcher)
    exportResults(qid, query, subjects, "rm", searcher, rmRankings)

    println("Running timeslice queries")
    val pool = Seq[ScoredDocument]()
    for (decade <- minDate to maxDate by 10){
      val decadeRankings = ExpansionModels.runDecadeExpansionQuery(decade, galagoQuery, "robust", searcher)
      val decadeExpansionTerms = ExpansionModels.lce(decadeRankings take numExpansionDocs, searcher, numExpansionTerms)
      val decadeRmRankings = ExpansionModels.runExpansionQuery(galagoQuery, decadeExpansionTerms, "robust", searcher)
      pool ++ decadeRmRankings
    }
    exportResults(qid, query, subjects, "time", searcher, pool.sortBy(_.score) take numResults)
  }


  def exportResults(qid: Int, query : String, subjects: Map[String, String], runType: String,
                    searcher : GalagoSearcher, rankings: Seq[ScoredDocument])
  {
    println(s"Exporting $runType Results")
    // make dirs
    Seq("raw", "trec", "qrel").foreach(dir => new File(s"$output/$runType/$dir/").mkdirs())

    // various output formats
    val rawPrinter = new java.io.PrintWriter(s"$output/$runType/raw/${qid}_${subjects(query)}")
    val trecPrinter = new java.io.PrintWriter(s"$output/$runType/trec/${qid}_${subjects(query)}")
    val qrelPrinter = new java.io.PrintWriter(s"$output/$runType/qrel/${qid}_${subjects(query)}")
//    val qrelPrinter = new java.io.PrintWriter(new BufferedWriter(new FileWriter(s"$output/$runType/qrel", true)))
    // keep track of number of docs that pass output criteria
    var rank = 1
    val qSubject = subjects(query)
    try {
      rankings.foreach(rankedDoc => {
        // get some data to export
        val doc = searcher.pullDocumentWithTokensAndMeta(rankedDoc.documentName)
        val lang = doc.metadata.get("language")
        val subject = doc.metadata.get("subject")
        val year = doc.metadata.get("year")
        // make sure this doc has a valid year, mappable subject and is english
        if (year != null && subject != null && lang != null && subjects.contains(subject)
          && langRegex.pattern.matcher(lang).matches() && yearRegex.pattern.matcher(year).matches()) {
          // output
          rawPrinter.println(s"$qid \t ${rankedDoc.rank} \t ${rankedDoc.score} \t ${doc.name} \t $subject \t ${subjects(subject)} \t $year")
          trecPrinter.println("%s Q0 %s %d %s %s".format(qid, doc.name, rank, "%10.8f".format(rankedDoc.score), runType))
          // estimate relevance by subject heading
          var relevance = 0
          if (subject.charAt(0) == qSubject.charAt(0)) relevance += 2
          if (subject.charAt(1) == qSubject.charAt(1)) relevance += 2
          qrelPrinter.println("%s %s %s %s".format(qid, 0, doc.name, relevance))
          rank += 1
        }
      })
    }
    finally {
      rawPrinter.close()
      trecPrinter.close()
      qrelPrinter.close()
    }
  }
}