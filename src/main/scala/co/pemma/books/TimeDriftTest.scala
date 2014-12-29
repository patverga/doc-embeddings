package co.pemma.books

import java.io.File

import co.pemma.ExpansionModels
import co.pemma.embeddings.{WordVectorMath, WordVectorsSerialManager}
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable

/**
 * Created by pv on 12/17/14.
 */
object TimeDriftTest extends BookTimeSearcher {
  val expTerms = 1000
  val export1Terms = 10

  def main(args: Array[String]) {
    val (qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String, useLongQueries: Boolean) = initialize(args)
    val sdmQuery = GalagoQueryBuilder.seqdep(cleanQuery).queryStr

    searcher.getUnderlyingRetrieval()

    //    val minWordVecs = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors(s"./vectors/decade-vectors/$minDate.vectors.dat"))
    //    val wordVecExpansionTerms = minWordVecs.nearestNeighbors(minWordVecs.word2Vec()cleanQuery, usePhrases = false)
    //    wordVecExpansionTerms.foreach(println(_))

    // querying each decade seperately
    val decadeTerms = for (decade <- maxDate to minDate by -10)
    yield decade -> ExpansionModels.lce(ExpansionModels.runDecadeQuery(decade, sdmQuery, "robust", searcher, numResultDocs)
        take numExpansionDocs, searcher, expTerms * 2).filterNot(term => yearRegex.pattern.matcher(term._1).matches()).take(expTerms)

//    // querying all data at once
//    val sdmRankings = searcher.retrieveScoredDocuments(sdmQuery, None, numResultDocs)
//    val allTerms = (decadeTerms :+ (0 -> ExpansionModels.lce(sdmRankings take numExpansionDocs, searcher, expTerms * 2).
//      filterNot(term => yearRegex.pattern.matcher(term._1).matches()).take(expTerms))).toMap

    export1(decadeTerms.toMap.take(export1Terms), query, qid, useLongQueries)
    export2(decadeTerms.toMap, query, qid, useLongQueries)
  }

  def export1(termMap: Map[Int, Seq[(String, Double)]], query: String, qid: Int, longQueries: Boolean): Unit = {
    val dir = if (longQueries) "./books/output/query-drift/long-1" else "./books/output/query-drift/short-1"
    new File(dir).mkdirs()
    val printer = new java.io.PrintWriter(s"$dir/${query.replaceAll("\\s+", "_")}")

    val termMapSubset = termMap.map(d => (d._1, d._2 take export1Terms))
    val uniqueTerms = termMapSubset.flatMap(_._2).map(_._1).toSet.size
    val spanTerms = (termMapSubset(minDate) ++ termMapSubset(maxDate)).map(_._1).toSet.size

    println(uniqueTerms, spanTerms)
    try {
      termMapSubset.toSeq.sortBy(_._1).foreach { case (decade, terms) =>
        printer.write(decade + "\t")
        terms.foreach(t => printer.write(t._1 + "\t"))
        printer.write("\n")
      }
      printer.write(query + "\t" + uniqueTerms + "\t" + spanTerms + "\n")
    }
    finally {
      printer.close()
    }
  }

  def export2(termMap: Map[Int, Seq[(String, Double)]], query: String, qid: Int, longQueries: Boolean): Unit = {
    val dir = if (longQueries) "./books/output/query-drift/long-2" else "./books/output/query-drift/short-2"
    new File(dir).mkdirs()
    val printer = new java.io.PrintWriter(s"$dir/${query.replaceAll("\\s+", "_")}")

    val decades = termMap.keys.toSeq
    val drifts = for (i <- 0 to decades.size - 2)
      yield (decades(i), decadeDrift(termMap(decades(i)), termMap(decades(i + 1))))

    try {
      drifts.foreach{case (decade, drift) =>
        printer.write(s"$decade\t$drift\n")
      }
      printer.write(s"range\t${decadeDrift(termMap(decades.head), termMap(decades.last))}\n")
      printer.write(s"avg\t${drifts.map(_._2).sum/drifts.size}\n")
    }
    finally {
      printer.close()
    }
  }


  def decadeDrift(d1: Seq[(String, Double)], d2: Seq[(String, Double)]): Double =
  {
    val d1Map = new mutable.HashMap[String, Int]()
    d1.zipWithIndex.foreach{case(t2, rank2) =>
      d1Map.put(t2._1, rank2)
    }
    val d2Map = new mutable.HashMap[String, Int]()
    d2.zipWithIndex.foreach{case(t2, rank2) =>
        d2Map.put(t2._1, rank2)
    }

    var rankChange = 0
    d1.take(10).zipWithIndex.foreach{case(t1, rank1) =>
      val rank2 = d2Map.getOrElse(t1._1, d2.size)
      rankChange += Math.abs(rank1 - rank2)
    }
    d2.take(10).zipWithIndex.foreach{case(t1, rank1) =>
      val rank2 = d1Map.getOrElse(t1._1, d2.size)
      rankChange += Math.abs(rank1 - rank2)
    }
    rankChange / 20.0
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
