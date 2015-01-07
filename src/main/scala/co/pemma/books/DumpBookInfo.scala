package co.pemma.books

import java.io.File

import edu.umass.ciir.strepsimur.galago.GalagoSearcher
import org.lemurproject.galago.tupleflow.Parameters

import scala.io.Source

/**
 * Created by pv on 1/7/15.
 */
object DumpBookInfo extends BookTimeSearcher
{
  def main(args: Array[String])
  {
    val num = args(0)
    // set up galago
    val skipIndex = Seq(8, 28, 37, 38)
    assert(!skipIndex.contains(num))

//    val bookIndex =  s"./index/page-filtered-index_$num"
    val bookIndex =  s"./index/page-indices/page-filtered-index_$num"
    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    val searcher = GalagoSearcher(indexParam)

    val keys =  Source.fromFile(s"./data/keys/$num").getLines().toList

    val docNames = keys.map(key => searcher.getUnderlyingRetrieval().getDocumentName(key.toInt)).map(s => {
      val break = s.lastIndexOf('_')
      val book = s.substring(0,break)
      val page = s.substring(break+1).toInt
      (book, page)
    }).sortBy{case (book, page) => (book, page)}

    var lastBookName = ""
    var currentBookContent = new StringBuilder()
    docNames.foreach{ case(book, page) =>
      println(book, page)
      val doc = searcher.pullDocumentWithTokensAndMeta(book+"_"+page)
      val subject = doc.metadata.get("subject")

      // new book
      if (lastBookName.nonEmpty && lastBookName != book) {
        export(currentBookContent, book, subject)
        currentBookContent = new StringBuilder()
      }

      lastBookName = book
      currentBookContent.append(doc.text)
    }

  }


  def export(bookContent : StringBuilder, book : String, subject : String): Unit = {
    val dir = s"./data/books_raw/$subject"
    new File(dir).mkdirs()
    val printer = new java.io.PrintWriter(s"$dir/$book")
    try {
        printer.write(bookContent.toString())
    }
    finally {
      printer.close()
    }
  }

}
