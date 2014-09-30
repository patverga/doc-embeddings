package co.pemma

import java.io.{FileInputStream, BufferedInputStream}
import java.util.zip.GZIPInputStream

import cc.factorie.app.nlp
import cc.factorie.app.nlp.load.LoadPlainText
import cc.factorie.app.nlp.{segment, DocumentAnnotationPipeline}

import scala.io.Source
import scala.xml.XML

/**
 * Created by pat on 9/2/14.
 */
object DocReader
{

  def parseRobust(robustXml : String) : String =
  {
    val headline = if (robustXml.contains("<HEADLINE>")){
      val headLineSection = robustXml.split("<HEADLINE>",2)(1)
      headLineSection.split("</HEADLINE>",2)(0)}
    else ""
    val textSection = robustXml.split("<TEXT>",2)(1)
    val text = textSection.split("</TEXT>",2)(0)
    // combine headline and body, strip out remaining tags such as <P>
    (headline + " " + text).replaceAll("<.*>","")

  }

  def readNaNewsData(inputLoc : String) : Seq[String] =
  {
    val source = Source.fromFile(inputLoc, "iso-8859-1")
    print("Loading Docs from file...")
    val lines = source.getLines().toSeq.filter(_!="<p>")

    val lineSplit = lines.mkString("\n").split("<TEXT>").drop(1).map(_.split("</TEXT>")(0))
    val removeStart = lineSplit.map(line =>
    {
      val lowerLine = line.stripLineEnd.toLowerCase
      if (lowerLine.contains("&md;"))
        lowerLine.split("&md;")(1)
      else lowerLine
    })
    println(" Done.")
    removeStart
  }

  def readReuters(inputLoc : String) : (String, Seq[String]) =
  {
    val fullDoc = Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(inputLoc))))
    val (text, codes) = fullDoc.getLines().filter(line => line.startsWith("<p>") || line.startsWith("  <code ")).toSeq.partition(_.startsWith("<p>"))
    val textString = text.map(s => s.substring(3, s.length-4)).mkString("\n")
    val codeStrings = codes.map(c => c.substring(c.indexOf("\"")+1, c.indexOf(">")-1))
    print(codeStrings.mkString(", "))
    (textString, codeStrings)
  }
}
