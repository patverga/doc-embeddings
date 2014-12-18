package co.pemma.books

import co.pemma.ExpansionModels
import co.pemma.embeddings.{WordVectorsSerialManager, WordVectorMath}
import edu.umass.ciir.strepsimur.galago.{GalagoQueryLib, GalagoQueryBuilder, GalagoSearcher}
import org.apache.commons.lang3.StringUtils

/**
 * Created by pv on 12/17/14.
 */
object TimeDriftTest extends BookTimeSearcher
{
  def main(args: Array[String]) {
    val (qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String) = initialize(args)
    val sdmQuery = GalagoQueryBuilder.seqdep(cleanQuery).queryStr

    val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors("./vectors/decade-vectors/180-194.vectors.dat"))
    val wordVecExpansionTerms = wordVecs.stringNearestNeighbors(cleanQuery)
    wordVecExpansionTerms.foreach(println(_))

    println("Running timeslice queries...")
    val decadeTerms = for (decade <- maxDate to minDate by -10)
//      sdmRankings = ExpansionModels.runDecadeQuery(maxDate, sdmQuery, "robust", searcher, numResultDocs);
//      decadeExpansionTerms = ExpansionModels.lce(sdmRankings take numExpansionDocs, searcher, numExpansionTerms).
//        filterNot(term => yearRegex.pattern.matcher(term._1).matches());
     yield  ExpansionModels.lce(ExpansionModels.runDecadeQuery(maxDate, sdmQuery, "robust", searcher, numResultDocs)
        take numExpansionDocs, searcher, numExpansionTerms).filterNot(term => yearRegex.pattern.matcher(term._1).matches());
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
