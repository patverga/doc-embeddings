package co.pemma.books

import java.io.{File, PrintWriter, BufferedWriter, FileWriter}

/**
 * Created by pat on 10/9/14.
 */
object BookNer extends App
{

  val bookYear = loadMap("books/book-year-key")
  val bookDir = loadMap("books/book-ner-dir-key")

  bookYear.zipWithIndex.foreach{case((book, year),i) =>
    val inFile = bookDir.getOrElse(book, null)
    if (inFile != null){
      val outFile = s"books/$year/$book"
      println(inFile, outFile)
      loadBookFromOwpl(inFile, outFile)
    }
  }

  def loadMap(file : String): Map[String, String] =
  {
    val source = scala.io.Source.fromFile(file)
    val map = source.getLines().map(line => {
      val parts = line.split("\\s+")
      if (parts.length == 2) parts(0) -> parts(1)
      else null
    }).filterNot(_ == null).toMap
    source.close()
    map
  }


  def loadBookFromOwpl(inFile : String, outFile:String): Unit =
  {
    val source = scala.io.Source.fromFile(inFile)
    val lines = source.getLines()

    // export the books into date directories
    val file = new File(outFile)
    file.mkdirs()
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(file, true)))
    var sentence = new StringBuilder
    while (lines.hasNext)
    {
      val line = lines.next()
      // new sentence
      if (line == "")
      {
        if (sentence.nonEmpty)
          writer.println(sentence.toString())
        sentence = new StringBuilder
      }
      else {
        val tokens = line.split("\\s+")
        sentence.append(tokens(2).replaceAll("[^\\p{ASCII}]", ""))
        sentence.append(" ")
      }
    }
    source.close()
    writer.close()
  }
}
