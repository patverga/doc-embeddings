package co.pemma.books

import java.io.File

import cc.factorie.app.nlp.embeddings.EmbeddingOpts
import cc.factorie.util.CmdOptions
import co.pemma.ExpansionModels
import co.pemma.embeddings.{WordVectorMath, WordVectorsSerialManager}
import edu.umass.ciir.strepsi.StopWordList
import edu.umass.ciir.strepsimur.galago.stopstructure.StopStructuring
import edu.umass.ciir.strepsimur.galago.{GalagoQueryLib, GalagoQueryBuilder, GalagoSearcher}
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.tupleflow.Parameters
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by pv on 11/19/14.
 */
object BookQueries extends  BookTimeSearcher{
  def main(args: Array[String])
  {
    val (qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String, useLongQueries : Boolean) = initialize(args)
    val sdmQuery = GalagoQueryBuilder.seqdep(cleanQuery).queryStr

//    println("\nRunning QL Query...")
//    val qlRankings = searcher.retrieveScoredDocuments(s"#combine($cleanQuery)", None, numResultDocs)
//    exportResults(qid, query, subjects, "ql", searcher, qlRankings)
//
//
//    println("\nRunning SDM Query...")
    val sdmRankings = searcher.retrieveScoredDocuments(sdmQuery, None, numResultDocs)
//    exportResults(qid, query, subjects, "sdm", searcher, sdmRankings)
//
//
//    println("\nRunning RM Query...")
    val rmExpansionTerms = ExpansionModels.lce(sdmRankings take numExpansionDocs, searcher, numExpansionTerms).
      filterNot(term => yearRegex.pattern.matcher(term._1).matches())
//    val rmRankings = ExpansionModels.runExpansionQuery(sdmQuery, rmExpansionTerms, "robust", searcher, numResultDocs)
//    exportResults(qid, query, subjects, "rm", searcher, rmRankings)
//
//    println("\nRunning single word embedding queries...")
    val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors("./vectors/decade-vectors/180-194.vectors.dat"))
//    val wordVecExpansionTerms = wordVecs.stringNearestNeighbors(cleanQuery, filter = true, usePhrases = false)
////    val wordVecRankings = ExpansionModels.runExpansionQuery(sdmQuery, wordVecExpansionTerms.map((_,1.0)), "robust", searcher, numResultDocs)
//    val wordVecQuery = wordVecExpansionTerms.mkString("#sdm("," ", ")")
//    println(wordVecQuery)
//    val wordVecRankings = searcher.retrieveScoredDocuments(wordVecQuery, None, numResultDocs)
//    exportResults(qid, query, subjects, "wordvecs", searcher, wordVecRankings)
//
//    println("\nRunning old word embedding queries...")
//    val wvExpansionTerms = wordVecs.oldStringNearestNeighbors(cleanQuery, filter = true)
//    println(wvExpansionTerms.mkString("\n"))
//    val wvRankings = ExpansionModels.runExpansionQuery(sdmQuery, wvExpansionTerms.map((_,1.0)), "robust", searcher, numResultDocs)
//    exportResults(qid, query, subjects, "wordvecs-old", searcher, wvRankings)
//
//    println("\nRunning hybrid word embedding queries...")
//    val wv3ExpTerms = wordVecs.stringNearestNeighbors(cleanQuery, filter = true, usePhrases = false)
//    val wv3Rankings = ExpansionModels.runExpansionQuery(sdmQuery, wv3ExpTerms.map((_,1.0)), "robust", searcher, numResultDocs)
//    exportResults(qid, query, subjects, "wordvecs-hybrid", searcher, wv3Rankings)

    println("\nRunning RM embedding Query...")
    val rmWordVecExpansionTerms = wordVecs.stringNearestNeighbors(rmExpansionTerms.map(_._1).mkString(" "), filter = true, usePhrases = false)
    val weightedExp = (rmWordVecExpansionTerms, rmExpansionTerms.map(_._2)).zipped.map( (_, _) )
    val rm2Rankings = ExpansionModels.runExpansionQuery(sdmQuery, weightedExp, "robust", searcher, numResultDocs)
    exportResults(qid, query, subjects, "rm", searcher, rm2Rankings)


//    println("Running timeslice queries..."
//    val pool = ListBuffer[ScoredDocument]()
//    var lastRankings = ExpansionModels.runDecadeExpansionQuery(maxDate, sdmQuery, "robust", searcher, numResultDocs)
//    for (decade <- maxDate to minDate by -10){
//      val decadeExpansionTerms = ExpansionModels.lce(lastRankings take numExpansionDocs, searcher, numExpansionTerms).
//        filterNot(term => yearRegex.pattern.matcher(term._1).matches())
//      val decadeRmRankings = ExpansionModels.runDecadeExpansionQuery(decade,
//        GalagoQueryLib.buildWeightedCombine(Seq((sdmQuery, 0.55), (GalagoQueryLib.buildWeightedCombine(
//          decadeExpansionTerms take numExpansionTerms), 1 - 0.55))),
//        "robust", searcher)
//      pool ++= decadeRmRankings
//      lastRankings = decadeRmRankings
//    }
//    exportResults(qid, query, subjects, "time-rm", searcher, pool.sortBy(_.score) take numResultDocs)
//
//
//    println("Running time slice word embedding queries...")
//    val decadeVecPool = ListBuffer[ScoredDocument]()
//    for (decade <- minDate to maxDate by 10) {
//      val decadeVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors(s"./vectors/decade-vectors/${decade/10}.vectors.dat"))
//      val decadeVecExpansionTerms = decadeVecs.stringNearestNeighbors(cleanQuery, filter = true).map((_,1.0))
//      val decadeVecRankings = ExpansionModels.runDecadeExpansionQuery(decade,
//        GalagoQueryLib.buildWeightedCombine(Seq((sdmQuery, 0.55), (GalagoQueryLib.buildWeightedCombine(
//          decadeVecExpansionTerms take numExpansionTerms), 1 - 0.55))), "robust", searcher)
//      decadeVecPool ++= decadeVecRankings
//    }
//    exportResults(qid, query, subjects, "time-vectors", searcher, decadeVecPool.sortBy(_.score) take numResultDocs)

  }
}

class BookTimeSearcher{
  // set some params
  val numResultDocs = 2500
  val numExpansionDocs = 10
  val numExpansionTerms = 50
  var output = "./books/output/"
  val langRegex = "[eE]ng(?:lish)?".r
  val yearRegex = "[12][0-9]{3}".r
  val minDate = 1800
  val maxDate = 1940

  def initialize(args: Array[String]): (Int, String, Map[String, String], GalagoSearcher, String, Boolean) =
  {
    assert(args.size > 0, " Must supply a query id number.")
    val opts = new BookQueryOpts()
    opts.parse(args)
    val qid = opts.qid.value

    // read in queries
    val queryFile = if (opts.useLongQueries.value) "/book_long_queries_50" else "/book_queries_5"
    output += queryFile + "/"
    val querySource = Source.fromURL(getClass.getResource(queryFile))(io.Codec("UTF-8"))
    val queries = querySource.getLines().toList
    querySource.close()
    val query = queries(qid)

    // read in subjectmap
    val subjectSource = Source.fromURL(getClass.getResource("/subject-id-map"))(io.Codec("UTF-8"))
    val subjects = subjectSource.getLines().map(line => {
      val parts = line.split("\\|")
      parts(0) -> parts(1)
    }).toMap
    subjectSource.close()

    // make sure this subject is mappable
    assert(subjects.contains(query), s"The query \'$query\' does not exist in the subjects map.")
    println(s" Running query number $qid: $query")

    // set up galago
    val skipIndex = Seq(8, 28, 37, 38)
    val bookIndex = if (opts.test.value) List("./index/page-filtered-index_02").asJava
    //      else{ (for (i <- 0 to 20; if i != 14; num = if (i < 10) s"0$i"; else s"$i")
    //        yield s"/work2/manmatha/michaelz/galago/Proteus/Proteus/homer/mzShards/pages-index_$num").toList.asJava
    else {
      (for (i <- 0 to 43; if !skipIndex.contains(i); num = if (i < 10) s"0$i"; else s"$i")
      yield s"./index/page-indices/page-filtered-index_$num").toList.asJava
    }
    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    val searcher = GalagoSearcher(indexParam)
    val cleanQuery = GalagoQueryLib.normalize(query.toLowerCase).filterNot(StopWordList.isStopWord).mkString(" ")
    (qid, query, subjects, searcher, cleanQuery, opts.useLongQueries.value)
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
    val qSubjectID = subjects(query)
    try {
      rankings.foreach(rankedDoc => {
        // get some data to export
        val doc = searcher.pullDocumentWithTokensAndMeta(rankedDoc.documentName)
        val lang = doc.metadata.get("language")
        val subject = doc.metadata.get("subject")
        val year = doc.metadata.get("date")
        val intYear = Integer.parseInt(year)
        // make sure this doc has a valid year within the given range, mappable subject and is english
        if (year != null && minDate <= intYear && intYear <= maxDate && subject != null && lang != null && subjects.contains(subject)
          && langRegex.pattern.matcher(lang).matches() && yearRegex.pattern.matcher(year).matches())
        {
          val subjectID = subjects(subject)
          rawPrinter.println(s"$qid \t ${rankedDoc.rank} \t ${rankedDoc.score} \t ${doc.name} \t $subject \t $subjectID \t $year")
          trecPrinter.println("%s Q0 %s %d %s %s".format(qid, doc.name, rank, "%10.8f".format(rankedDoc.score), runType))
          // estimate relevance by subject heading
          var relevance = 0
          if (subjectID.charAt(0) == qSubjectID.charAt(0)) {
//            relevance += 2
            if (subjectID.charAt(1) == qSubjectID.charAt(1))
              relevance += 2
          }
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

  class BookQueryOpts extends CmdOptions
  {
    val qid = new CmdOption("qid", 0, "INT", "Query id number.")
    val test = new CmdOption("test", false, "BOOLEAN", "Use small subindex for testing.")
    val useLongQueries = new CmdOption("long", false, "BOOLEAN", "Use long queries (short by default).")
  }
}