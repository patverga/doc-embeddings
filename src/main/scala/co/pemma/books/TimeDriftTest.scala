package co.pemma.books

import co.pemma.embeddings.{WordVectorsSerialManager, WordVectorMath}
import edu.umass.ciir.strepsimur.galago.GalagoSearcher
import org.apache.commons.lang3.StringUtils

/**
 * Created by pv on 12/17/14.
 */
object TimeDriftTest extends BookTimeSearcher
{
  def main(args: Array[String]) {
    val (qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String) = initialize(args)
    val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors("./vectors/decade-vectors/180-194.vectors.dat"))
    val wordVecExpansionTerms = wordVecs.stringNearestNeighbors(cleanQuery)
    wordVecExpansionTerms.foreach(println(_))
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
