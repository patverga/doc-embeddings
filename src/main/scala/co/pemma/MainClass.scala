package main.scala.co.pemma

import cc.factorie.app.nlp.embeddings.{EmbeddingDistance, SkipGramNegSamplingEmbeddingModel}
import cc.factorie.app.nlp.load.LoadPlainText
import cc.factorie.app.nlp.{DocumentAnnotationPipeline, segment}
import cc.factorie.util.CmdOptions

/**
 * Created by pv on 8/28/14.
 */
object MainClass  extends  App{

  val inputDir = "./data/clean"
  val corpus = "./data/corpus"
  val tokenizedDir = "./data/tokenized"

  tokenizeText()
//  createEmbeddings()
//  interactiveDistance()


  def tokenizeText()
  {
    val outputDir = "data/tokenized"
    val pipeline = new DocumentAnnotationPipeline(
      Seq(segment.DeterministicTokenizer, segment.PlainTokenNormalizer))

    new java.io.File(inputDir).listFiles.toSeq.par.foreach(file => {
      println(s"Reading in raw text from \'$file\'. ")
      //    val lines = io.Source.fromInputStream(new FileInputStream(corpus)).
      val str = io.Source.fromFile(file).getLines.mkString("\n")
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
      "--threads=48",
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

  class EmbeddingOpts extends CmdOptions {

    // Algorithm related
    val dimension = new CmdOption("size", 200, "INT", "use <int> size of word vectors")
    val window = new CmdOption("window", 5, "INT", "use <int> skip length between words")
    val threads = new CmdOption("threads", 12, "INT", "use <int> threads")
    val negative = new CmdOption("negative", 1, "INT", "use <int> number of negative examples")
    val minCount = new CmdOption("min-count", 5, "INT", "This will discard words that appear less than <int> times; default is 5")
    val ignoreStopWords = new CmdOption("ignore-stopwords", false, "BOOLEAN", "use <bool> to include or discard stopwords. Use 1 for discarding stopwords")
    val cbow = new CmdOption("cbow", false, "BOOLEAN", "user cbow=true for cbow and cbow=false for skip-gram") // 1 would be SkipGram // default method is skipgram
    val sample = new CmdOption("sample", 0.001, "DOUBLE", "use <double> subsampling")

    // Optimization related (Don't change if you do not understand how vectors are initialized)
    val rate = new CmdOption("rate", 0.025, "DOUBLE", "learning rate for adaGrad")
    val delta = new CmdOption("delta", 0.1, "DOUBLE", "delta for adaGrad")

    // IO Related (MUST GIVE Options)
    val encoding = new CmdOption("encoding", "UTF8", "STRING", "use <string> for encoding option. ISO-8859-15 is default")
    val saveVocabFile = new CmdOption("save-vocab", "", "STRING", "save vocab file")
    val loadVocabFile = new CmdOption("load-vocab", "", "STRING", "load the vocab file") // atleast one of them  should be given. save-vocab or load-vocab
    val corpus = new CmdOption("train", "", "STRING", "train file")
    val output = new CmdOption("output", "", "STRING", "Use <file> to save the resulting word vectors")
    val binary = new CmdOption("binary", false, "BOOLEAN", "use true for storing .gz format and false for plain txt format. Both stores in ISO-8859-15 Encoding")

    // Vocabulary related
    // Maximum 14.3M * 0.7 = 10M words in the vocabulary (Don;t change if you understand how vocabBuilder works)
    val vocabSize = new CmdOption("max-vocab-size", 2e6.toInt, "INT", "Max Vocabulary Size. Default Value is 2M . Reduce to 200k or 500k is you learn embeddings on small-data-set")
    val vocabHashSize = new CmdOption("vocab-hash-size", 14.3e6.toInt, "INT", "Vocabulary hash size")
    val samplingTableSize = new CmdOption("sampling-table-size", 1e8.toInt, "INT", "Sampling Table size")


  }
}
