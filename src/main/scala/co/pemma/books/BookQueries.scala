package co.pemma.books

import java.io.File

import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.tupleflow.Parameters

import scala.io.Source

/**
 * Created by pv on 11/19/14.
 */
object BookQueries
{
  // set some params
  val numDocs = 50000
  val output = "./books/output/"
  val langRegex = "[eE]ng(?:lish)?".r

  def main(args: Array[String])
  {
    assert(args.size > 0, " Must supply a query id number.")
    val qid = Integer.parseInt(args(0))

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
    //      val bookIndex = List("./index/books-index_small", "./index/wikipedia").asJava
    val bookIndex = "./index/pages-index_20"
//    val bookIndex = (for (i <- 0 to 20; if i != 14; num = if (i < 10) s"0$i"; else s"$i")
//    yield s"/work2/manmatha/michaelz/galago/Proteus/Proteus/homer/mzShards/pages-index_$num").toList.asJava
    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    val searcher = GalagoSearcher(indexParam)
    val defaultStopStructures = new StopStructuring(searcher.getUnderlyingRetrieval())

    val unstoppedQuery = defaultStopStructures.removeStopStructure(query)
    // run sdm and rm queries and export results
    Seq(("sdm", GalagoQueryBuilder.seqdep(unstoppedQuery).queryStr),
      ("rm", s"#rm($unstoppedQuery)"))
      .foreach{case (qType, gQuery) =>
      val rankings = searcher.retrieveScoredDocuments(gQuery, None, numDocs)
      exportResults(qid, query, subjects, rankings, qType, searcher)
    }
  }

  def exportResults(qid: Int, query : String, subjects: Map[String, String],
                    rankings: Seq[ScoredDocument], runType: String, searcher : GalagoSearcher) {
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
        // make sure this doc has a year, subject and is english
        if (year != null && subject != null && lang != null && subjects.contains(subject) && langRegex.pattern.matcher(lang).matches()) {
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