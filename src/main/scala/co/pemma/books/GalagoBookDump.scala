package co.pemma.books

import java.io.{File, PrintWriter}

import edu.umass.ciir.strepsimur.galago.GalagoSearcher
import org.lemurproject.galago.utility.Parameters

import scala.util.matching.Regex

/**
 * Created by pat on 10/8/14.
 */
object GalagoBookDump extends App {

  val indexLocation = "./index/books_00"
  val galagoSearcher: GalagoSearcher = {
    val indexParam = new Parameters()
    indexParam.set("index", indexLocation)
    GalagoSearcher(indexParam)
  }

  val allBooks = galagoSearcher.retrieveScoredDocuments("the", resultCount = 100000)
  println(s"Found ${allBooks.size} books in index.")
  val validDateRegex = new Regex("^[0-9]{4}$").pattern
  // get the books with valid dates
  allBooks.zipWithIndex.foreach({ case (bookId, i) =>
    if ((i+1) % 100 == 0) println(i+1)
    val doc = galagoSearcher.pullDocumentWithTokensAndMeta(bookId.documentName)
    val date = doc.metadata.get("date")
    if (date != null && validDateRegex.matcher(date).matches() && doc.metadata.get("language") == "eng") {
      // export the books into date directories
      val file = new File(s"books/$date")
      file.mkdirs()
      val writer = new PrintWriter(s"books/$date/${doc.name}")
      writer.println(doc.text)
      writer.close()
    }
  })
}
