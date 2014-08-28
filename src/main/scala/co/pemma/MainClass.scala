package main.scala.co.pemma

import cc.factorie.app.nlp.embeddings.{EmbeddingDistance, EmbeddingOpts, SkipGramNegSamplingEmbeddingModel}
import cc.factorie.app.nlp.load.LoadPlainText
import cc.factorie.app.nlp.{DocumentAnnotationPipeline, segment}

/**
 * Created by pv on 8/28/14.
 */
object MainClass  extends  App{

  val corpus = "input.clean"

  tokenizeText()

  def tokenizeText()
  {
    val inputDir = "./data/clean"
    val outputDir = "data/tokenized"
    val pipeline = new DocumentAnnotationPipeline(
      Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))

    val docs = new java.io.File(inputDir).listFiles.map(file => {
      println(s"Reading in raw text from \'$file\'. ")
      //    val lines = io.Source.fromInputStream(new FileInputStream(corpus)).
      val str = io.Source.fromFile(file).getLines.mkString
      (file, LoadPlainText.fromString(str).head)
    }).toSeq

    docs.par.foreach(doc => {
      pipeline.process(doc._2)
    })

    docs.foreach(fileDoc =>
    {
      val file = fileDoc._1
      val doc = fileDoc._2
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
    val opts = new EmbeddingOpts()
    opts.parse(Seq(
      "--ignore-stopwords=true",
      "--threads=7",
      "--encoding=UTF8",
      "--save-vocab=./vocab",
      //    "--load-vocab=./vocab",
      "--output=./vectors",
      s"--train=./$corpus"))
    val wordEmbedModel = new SkipGramNegSamplingEmbeddingModel(opts)
    wordEmbedModel.buildVocab()
    wordEmbedModel.learnEmbeddings()
    wordEmbedModel.store()
  }

  def interactiveDistance() {
    EmbeddingDistance.nearestNeighbours("vector", 5)
  }
}
