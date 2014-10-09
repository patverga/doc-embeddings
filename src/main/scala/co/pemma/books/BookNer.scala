package co.pemma.books

import java.io.{PrintWriter, BufferedWriter, FileWriter}

/**
 * Created by pat on 10/9/14.
 */
object BookNer extends App
{

  val file = "/home/pat/doc-embeddings/books/NER/0529370.0036.011.umich.edu.out"
  loadBookFromOwpl(file, "test")

  def loadBookFromOwpl(inFile : String, outFile:String): Unit =
  {
    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines()

    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outFile, true)))
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
