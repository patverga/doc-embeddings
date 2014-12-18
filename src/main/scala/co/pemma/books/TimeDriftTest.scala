package co.pemma.books

import java.io.File

import co.pemma.ExpansionModels
import co.pemma.embeddings.{WordVectorMath, WordVectorsSerialManager}
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import org.apache.commons.lang3.StringUtils

/**
 * Created by pv on 12/17/14.
 */
object TimeDriftTest extends BookTimeSearcher
{
  val expTerms = 10

  def main(args: Array[String]) {
    val (qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String) = initialize(args)
    val sdmQuery = GalagoQueryBuilder.seqdep(cleanQuery).queryStr

//    val minWordVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors(s"./vectors/decade-vectors/$minDate.vectors.dat"))
//    val wordVecExpansionTerms = minWordVecs.nearestNeighbors(minWordVecs.word2Vec()cleanQuery, usePhrases = false)
//    wordVecExpansionTerms.foreach(println(_))

    // querying each decade seperately
    val decadeTerms = for (decade <- maxDate to minDate by -10)
     yield decade ->ExpansionModels.lce(ExpansionModels.runDecadeQuery(decade, sdmQuery, "robust", searcher, numResultDocs)
        take numExpansionDocs, searcher, expTerms*2).filterNot(term => yearRegex.pattern.matcher(term._1).matches()).take(expTerms)

    // querying all data at once
    val sdmRankings = searcher.retrieveScoredDocuments(sdmQuery, None, numResultDocs)
    val allTerms = (decadeTerms :+ (0 -> ExpansionModels.lce(sdmRankings take numExpansionDocs, searcher, expTerms*2).
      filterNot(term => yearRegex.pattern.matcher(term._1).matches()).take(expTerms))).toMap

    export(decadeTerms.toMap, query, qid)
  }

  def export(termMap : Map[Int, Seq[(String, Double)]], query : String, qid : Int): Unit =
  {
    new File("./books/output/query-drift/").mkdirs()
    val printer = new java.io.PrintWriter(s"./books/output/query-drift/$qid")

    val uniqueTerms = termMap.flatMap(_._2).map(_._1).toSet.size
    val spanTerms = (termMap(minDate)++termMap(maxDate)).map(_._1).toSet.size

    println(uniqueTerms, spanTerms)
    try {
      termMap.toSeq.sortBy(_._1).foreach{case (decade, terms) =>
        printer.write(decade +"\t")
        terms.foreach(t => printer.write(t._1 + "\t"))
        printer.write("\n")
      }
      printer.write(query + "\t" + uniqueTerms + "\t" + spanTerms)
    }
    finally {
      printer.close()
    }
  }
}

object LevTest extends App{
  val a = "confucianism"
  val b = "buddhism"
  val c = "shinto"
  val d = "bud_dhism"
  val e = "buddhistic"
  println (StringUtils.getLevenshteinDistance(a, b).toDouble/((a.length + b.length).toDouble/2.0))
  println (StringUtils.getLevenshteinDistance(c, b).toDouble/((c.length + b.length).toDouble/2.0))
  println (StringUtils.getLevenshteinDistance(d, b).toDouble/((d.length + b.length).toDouble/2.0))
  println (StringUtils.getLevenshteinDistance(e, b).toDouble/((e.length + b.length).toDouble/2.0))

}
