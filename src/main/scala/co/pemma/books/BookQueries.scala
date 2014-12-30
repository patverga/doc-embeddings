package co.pemma.books

import java.io.File
import java.util

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
    val (qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String, querySet : String, editDistThreshold : Double) = initialize(args)
    val sdmQuery = GalagoQueryBuilder.seqdep(cleanQuery).queryStr

    baseLines(qid, query, subjects, searcher, cleanQuery, sdmQuery)

    val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors("./vectors/decade-vectors/180-194.vectors.dat"))

    wordVec1(qid, query, subjects, searcher, cleanQuery, sdmQuery, wordVecs, editDistThreshold)
    wordVec2(qid, query, subjects, searcher, cleanQuery, sdmQuery, wordVecs, editDistThreshold)
    wordVec3(qid, query, subjects, searcher, cleanQuery, sdmQuery, wordVecs, editDistThreshold)


    //    println("\nRunning RM embedding Query...")
//    val rmWordVecExpansionTerms = wordVecs.stringNearestNeighbors(rmExpansionTerms.map(_._1).mkString(" "), filter = true, usePhrases = false)
//    val weightedExp = (rmWordVecExpansionTerms, rmExpansionTerms.map(_._2)).zipped.map( (_, _) )
//    val rm2Rankings = ExpansionModels.runExpansionQuery(sdmQuery, weightedExp, "robust", searcher, numResultDocs)
//    exportResults(qid, query, subjects, "rm-wordvec", searcher, rm2Rankings)


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

  def baseLines(qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String, sdmQuery: String) {
    println("\nRunning QL Query...")
    val qlRankings = searcher.retrieveScoredDocuments(s"#combine($cleanQuery)", None, numResultDocs)
    exportResults(qid, query, subjects, "ql", searcher, qlRankings)

    println("\nRunning SDM Query...")
    val sdmRankings = searcher.retrieveScoredDocuments(sdmQuery, None, numResultDocs)
    exportResults(qid, query, subjects, "sdm", searcher, sdmRankings)

    println("\nRunning RM Query...")
    val rmExpansionTerms = ExpansionModels.lce(sdmRankings take numExpansionDocs, searcher, numExpansionTerms).
      filterNot(term => digitRegex.pattern.matcher(term._1).matches())
    val (rmRankings, _) = ExpansionModels.runExpansionQuery(sdmQuery, rmExpansionTerms, "robust", searcher, numResultDocs)
    exportResults(qid, query, subjects, "rm", searcher, rmRankings)
  }

  def wordVec1(qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String,
               sdmQuery: String, wordVecs: WordVectorMath, editDistThreshold : Double) {
    println("\nRunning word embedding 1 queries...")
    val wv1ExpTerms = wordVecs.oldStringNearestNeighbors(cleanQuery, filter = true, threshold = editDistThreshold)
    println(wv1ExpTerms.mkString("\n"))
    val (wv1Rankings, wv1Query) = ExpansionModels.runExpansionQuery(sdmQuery, wv1ExpTerms.map((_, 1.0)), "robust", searcher, numResultDocs)
    exportResults(qid, query, subjects, "wordvecs-1", searcher, wv1Rankings)

    println("\nRunning word embeddings 1 -> rm queries...")
    val wv1RmExpTerms = ExpansionModels.lce(wv1Rankings take numExpansionDocs, searcher, numExpansionTerms).
      filterNot(term => digitRegex.pattern.matcher(term._1).matches())
    val (wv1RmRankings,_) = ExpansionModels.runExpansionQuery(wv1Query, wv1RmExpTerms, "robust", searcher, numResultDocs)
    exportResults(qid, query, subjects, "wordvecs-1-rm", searcher, wv1RmRankings)
  }

  def wordVec2(qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String,
               sdmQuery: String, wordVecs: WordVectorMath, editDistThreshold : Double) {
    println("\nRunning word embedding 2 queries...")
    val wv2ExpTerms = wordVecs.stringNearestNeighbors(cleanQuery, filter = true, usePhrases = false, threshold = editDistThreshold)
    val wv2Query = wv2ExpTerms.mkString("#sdm(", " ", ")")
    println(wv2Query)
    val wv2Rankings = searcher.retrieveScoredDocuments(wv2Query, None, numResultDocs)
    exportResults(qid, query, subjects, "wordvecs-2", searcher, wv2Rankings)

    println("\nRunning word embeddings 2 -> rm queries...")
    val wv2RmExpTerms = ExpansionModels.lce(wv2Rankings take numExpansionDocs, searcher, numExpansionTerms).
      filterNot(term => digitRegex.pattern.matcher(term._1).matches())
    val (wv2RmRankings,_) = ExpansionModels.runExpansionQuery(wv2Query, wv2RmExpTerms, "robust", searcher, numResultDocs)
    exportResults(qid, query, subjects, "wordvecs-2-rm", searcher, wv2RmRankings)
  }

  def wordVec3(qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String,
               sdmQuery: String, wordVecs: WordVectorMath, editDistThreshold : Double) {
    println("\nRunning word embedding 3 queries...")
    val wv3ExpTerms = wordVecs.stringNearestNeighbors(cleanQuery, filter = true, usePhrases = false, threshold = editDistThreshold)
    val (wv3Rankings, wv3Query) = ExpansionModels.runExpansionQuery(sdmQuery, wv3ExpTerms.map((_, 1.0)), "robust", searcher, numResultDocs)
    exportResults(qid, query, subjects, "wordvecs-3", searcher, wv3Rankings)

    println("\nRunning word embeddings 3 -> rm queries...")
    val wv3RmExpTerms = ExpansionModels.lce(wv3Rankings take numExpansionDocs, searcher, numExpansionTerms).
      filterNot(term => digitRegex.pattern.matcher(term._1).matches())
    val (wv3RmRankings,_) = ExpansionModels.runExpansionQuery(wv3Query, wv3RmExpTerms, "robust", searcher, numResultDocs)
    exportResults(qid, query, subjects, "wordvecs-3-rm", searcher, wv3RmRankings)
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
  val digitRegex = "\\d+".r
  val minDate = 1800
  val maxDate = 1940

  def initialize(args: Array[String]): (Int, String, Map[String, String], GalagoSearcher, String, String, Double) =
  {
    assert(args.size > 0, " Must supply a query id number.")
    val opts = new BookQueryOpts()
    opts.parse(args)
    val qid = opts.qid.value

    // read in queries
    val queryFile = if (opts.querySet.value == "long" ) "/book_long_queries_50"
                    else if (opts.querySet.value == "3") "/n-3-subjects"
                    else "/book_queries_5"
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
    (qid, query, subjects, searcher, cleanQuery, opts.querySet.value, opts.editThreshold.value)
  }

  def exportResults(qid: Int, query : String, subjects: Map[String, String], runType: String,
                    searcher : GalagoSearcher, rankings: Seq[ScoredDocument])
  {
    println(s"Exporting $runType Results")
    // make dirs
    Seq("raw", "trec-page", "qrel-page", "trec-book", "qrel-book").foreach(dir => new File(s"$output/$runType/$dir/").mkdirs())

    // various output formats
    val rawPrinter = new java.io.PrintWriter(s"$output/$runType/raw/${qid}_${subjects(query)}")
    val trecPagePrinter = new java.io.PrintWriter(s"$output/$runType/trec-page/${qid}_${subjects(query)}")
    val qrelPagePrinter = new java.io.PrintWriter(s"$output/$runType/qrel-page/${qid}_${subjects(query)}")
    val trecBookPrinter = new java.io.PrintWriter(s"$output/$runType/trec-book/${qid}_${subjects(query)}")
    val qrelBookPrinter = new java.io.PrintWriter(s"$output/$runType/qrel-book/${qid}_${subjects(query)}")

    // keep track of number of docs that pass output criteria
    var pageRank = 1
    var bookRank = 1
    val usedBooks = new util.HashSet[String]
    val qSubjectID = subjects(query)

    try {
      rankings.foreach(rankedDoc => {
        // get some data to export
        val doc = searcher.pullDocumentWithTokensAndMeta(rankedDoc.documentName)
        val lang = doc.metadata.get("language")
        val subject = doc.metadata.get("subject")
        val year = doc.metadata.get("date")
        val intYear = Integer.parseInt(year)
        val book = rankedDoc.documentName.split("_", 2)(0)

        // make sure this doc has a valid year within the given range, mappable subject and is english
        if (year != null && minDate <= intYear && intYear <= maxDate && subject != null && lang != null && subjects.contains(subject)
          && langRegex.pattern.matcher(lang).matches() && yearRegex.pattern.matcher(year).matches())
        {
          val subjectID = subjects(subject)
          rawPrinter.println(s"$qid \t ${rankedDoc.rank} \t ${rankedDoc.score} \t ${doc.name} \t $subject \t $subjectID \t $year")

          // estimate relevance by subject heading
          val relevance = if (subjectID.charAt(0) == qSubjectID.charAt(0)) {
            if (subjectID.charAt(1) == qSubjectID.charAt(1)) 4 else 0 //2
            } else 0

          // export page results
          trecPagePrinter.println("%s Q0 %s %d %s %s".format(qid, doc.name, pageRank, "%10.8f".format(rankedDoc.score), runType))
          qrelPagePrinter.println("%s %s %s %s".format(qid, 0, doc.name, relevance))
          pageRank += 1

          // if this is the first time we've seen this book, rank it
          if (!usedBooks.contains(book)){
            trecBookPrinter.println("%s Q0 %s %d %s %s".format(qid, book, bookRank, "%10.8f".format(rankedDoc.score), runType))
            qrelBookPrinter.println("%s %s %s %s".format(qid, 0, book, relevance))
            usedBooks.add(book)
            bookRank += 1
          }
        }
      })
    }
    finally {
      rawPrinter.close()
      trecPagePrinter.close()
      trecBookPrinter.close()
      qrelPagePrinter.close()
      qrelBookPrinter.close()
    }
  }

  class BookQueryOpts extends CmdOptions
  {
    val qid = new CmdOption("qid", 0, "INT", "Query id number.")
    val editThreshold = new CmdOption("threshold", .25, "DOUBLE", "Edit distance threshold to use.")
    val test = new CmdOption("test", false, "BOOLEAN", "Use small subindex for testing.")
    val querySet = new CmdOption("query-set", "short", "STRING", "Use long, short or n3 queries (short by default).")
  }
}