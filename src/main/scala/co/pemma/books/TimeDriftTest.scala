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
  val a = "lincoln"
  val b = "lin cn"
  println (StringUtils.getLevenshteinDistance(a, b).toDouble/((a.length + b.length)/2))
  println (StringUtils.getLevenshteinDistance("cat", "sat").toDouble/3.)
}
