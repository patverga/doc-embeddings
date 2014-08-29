package main.scala.co.pemma

import cc.factorie.app.nlp.embeddings.{EmbeddingDistance, EmbeddingOpts, SkipGramNegSamplingEmbeddingModel}
import cc.factorie.app.nlp.load.LoadPlainText
import cc.factorie.app.nlp.{DocumentAnnotationPipeline, segment}

/**
 * Created by pv on 8/28/14.
 */
object MainClass  extends  App{

  val inputDir = "./data/clean"
  val corpus = "./data/corpus"
  val tokenizedDir = "./data/tokenized"

//  tokenizeText()
  createEmbeddings()
//  interactiveDistance()


  def tokenizeText()
  {
    val outputDir = "data/tokenized"
    val pipeline = new DocumentAnnotationPipeline(
      Seq(segment.DeterministicTokenizer, segment.PlainTokenNormalizer))

    new java.io.File(inputDir).listFiles.toSeq.par.foreach(file => {
      println(s"Reading in raw text from \'$file\'. ")
      //    val lines = io.Source.fromInputStream(new FileInputStream(corpus)).
      val str = io.Source.fromFile(file).getLines.mkString
      val doc = LoadPlainText.fromString(str).head

      println(s"Processing $file")
      pipeline.process(doc)

      val outputFile = s"$outputDir/${file.getName.substring(0, file.getName.indexOf(".conv"))}.tokenized"
      println(s" Writing to $outputFile")
      val writer = new java.io.PrintWriter(s"$outputFile")
      val tokString = doc.tokens.map(_.string).mkString(" ")
      writer.println(tokString)
      writer.close()
    })

    println("done")
  }


  def createEmbeddings() {
//    val inputs = new java.io.File(tokenizedDir).listFiles.toSeq.map(str => s" --train=$str")

    val opts = new EmbeddingOpts()
    opts.parse(Seq(
      "--ignore-stopwords=true",
      "--threads=7",
      "--encoding=UTF8",
      "--save-vocab=./vocab",
      //    "--load-vocab=./vocab",
      s"--train=./$corpus",
      "--output=./vectors"))

    val wordEmbedModel = new SkipGramNegSamplingEmbeddingModel(opts)
    wordEmbedModel.buildVocab()
    wordEmbedModel.learnEmbeddings()
    wordEmbedModel.store()
  }

  def interactiveDistance() {
    EmbeddingDistance.nearestNeighbours("vectors", 5)
  }
}
