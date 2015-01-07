package co.pemma

import java.io.{Writer, StringWriter}

import org.htmlcleaner.BaseToken
import org.htmlcleaner.CleanerProperties
import org.htmlcleaner.ContentNode
import org.htmlcleaner.HtmlCleaner
import org.htmlcleaner.HtmlSerializer
import org.htmlcleaner.TagNode
import org.htmlcleaner.Utils
import org.htmlcleaner._

/**
 * User: dietz
 * Date: 8/13/14
 * Time: 3:08 PM
 */
object HTMLTextCleaner {
  def textCleaner(text : String) : String = {
    val textNoExtents =
      text.toLowerCase.indexOf("<mentions>") match {
        case -1 => text
        case idx => text.substring(0, idx)
      }

    // HtmlCleaner has a tendency to go on vacations while parsing some hairy documents, particularly those produced by a certain company starting with M
    if (textNoExtents.substring(0, 500).contains("urn:schemas-microsoft-com:vml")) {
      ""
    } else {
      //TODO desperate measures falling back to galago tagtokenizer
      //      val textgclean = tokenizeText(textNoExtents).mkString(" ")
      cleanHTML(text)
      //val textClean = cleanHTML(textNoExtents)
    }
  }

  val cleaner = {
    val cleaner = new HtmlCleaner()
    // take default cleaner properties
    val props = cleaner.getProperties
    props.setAdvancedXmlEscape(true)
    //    props.setRecognizeUnicodeChars(true)
    //    props.setTransResCharsToNCR(false)
    props.setTranslateSpecialEntities(true)
    props.setCharset("UTF-8")
    props.setOmitComments(true)
    props.setPruneTags("script,style,img")
    props.setOmitHtmlEnvelope(true)
    props.setOmitCdataOutsideScriptAndStyle(true)
    props.setKeepWhitespaceAndCommentsInHead(false)
    props.setOmitDoctypeDeclaration(true)
    props.setOmitXmlDeclaration(true)

    cleaner
  }

  def cleanHTML(text:String):String = {
    //val textBody= text.substring(text.toLowerCase.indexOf("<body>"))
    //   val oneline = text.replaceAll("[\n\r]"," ").replaceAll("\\s+"," ")
    //   val regexCleaned= oneline//.substring(text.toLowerCase.indexOf("<body>"))
    //      .replaceAll("<script.*</script>"," ")
    //      .replaceAll("<[a-zA-Z/].*>"," ")
    //      .replaceAll("<[a-zA-Z].*>"," ")
    //   println(regexCleaned)

    val out =
      try {
        val node = cleaner.clean(text)
        val writer = new StringWriter()
        new SimpleTextSerializer(cleaner.getProperties).write(node, writer, "UTF-8", true)
        val out = writer.toString
        out
      } catch {
        case ex:IndexOutOfBoundsException => {
          System.err.println("Catching index(0) out of bounds exception from HtmlCleaner and dismissing document")
          ex.printStackTrace(System.err)
          ""
        }

      }
    //    val outNoUtf8 = java.text.Normalizer.normalize(out, java.text.Normalizer.Form.NFKD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    val transliterate =
      out.replaceAll("([A-Z])\\.([A-Z])\\.([A-Z])\\.([A-Z])\\.([A-Z])\\.","$1$2$3$4$5")
        .replaceAll("([A-Z])\\.([A-Z])\\.([A-Z])\\.([A-Z])\\.","$1$2$3$4")
        .replaceAll("([A-Z])\\.([A-Z])\\.([A-Z])\\.","$1$2$3")
        .replaceAll("([A-Z])\\.([A-Z])\\.","$1$2")
        .replaceAllLiterally(65533.toChar+"","'").replaceAllLiterally(8217.toChar+"","'")
        .replaceAllLiterally("&apos;","'")
        .replaceAllLiterally("&quot;","\"")
        .replaceAllLiterally("&gt;",">")
        .replaceAllLiterally("&lt;","<")
        .replaceAllLiterally("&amp;","&")
    //replaceAllLiterally("U.S.","US")
    transliterate
  }

  /**
   * <p>Simple HTML serializer - creates resulting HTML without indenting and/or compacting.</p>
   */
  class SimpleTextSerializer(props: CleanerProperties) extends HtmlSerializer(props) {

    protected def serialize(tagNode: TagNode, writer: Writer) {
      if (!isMinimizedTagSyntax(tagNode)) {
        import scala.collection.JavaConversions._
        for (item <- tagNode.getAllChildren) {
          item match {
            case _: ContentNode =>
              val content: String = item.toString
              writer.write(Utils.escapeXml(content,  props, false))
            //              writer.write(escapeText(content))
            case _ => item match {
              case token: BaseToken =>
                token.serialize(this, writer)
              case _ =>
            }
          }
        }
        writer.write(" ");
      }
    }
  }

}
