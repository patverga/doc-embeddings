package co.pemma.embeddings

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import cc.factorie.app.nlp.embeddings.{SkipGramNegSamplingEmbeddingModel, EmbeddingOpts}
import cc.factorie.la.DenseTensor1

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by pat on 9/26/14.
 */
class WordVectors extends Serializable
{
  var threshold = 0
  var unigrams = Array[String]()
  var bigrams = Array[String]()
  var trigrams = Array[String]()
  var weights = Array[DenseTensor1]()
  var D = 0
  var V = 0

  def loadFromTxt(embeddingsFile: String, encoding:String = "UTF8"): Unit = {
    val lineItr = Source.fromFile(embeddingsFile, encoding).getLines()
    // first line is (# words, dimension)
    val details = lineItr.next().stripLineEnd.split(' ').map(_.toInt)
    V = if (threshold > 0 && details(0) > threshold) threshold else details(0)
    D = details(1)
    println("# words : %d , # size : %d".format(V, D))
    weights = new Array[DenseTensor1](V)
    var uni = new ListBuffer[(String, DenseTensor1)]()
    var bi = new ListBuffer[(String, DenseTensor1)]()
    var tri = new ListBuffer[(String, DenseTensor1)]()
    for (v <- 0 until V)
    {
      val line = lineItr.next().stripLineEnd.split(' ')
      val word = line(0)

      val weight = new DenseTensor1(D, 0) // allocate the memory
      for (d <- 0 until D) weight(d) = line(d + 1).toDouble
      weight /= weight.twoNorm

      val underscoreCount = word.length() - word.replace("_", "").length()
      if (underscoreCount == 0)
        uni += Tuple2(word, weight)
      else if (underscoreCount == 1)
        bi += Tuple2(word, weight)
      else if (underscoreCount == 2)
        tri += Tuple2(word, weight)
    }
    unigrams = uni.zipWithIndex.map({case((wrd, wt), idx) => weights(idx) = wt;  wrd }).toArray
    bigrams = bi.zipWithIndex.map({case((wrd, wt), idx) => weights(idx + unigrams.length) = wt;  wrd }).toArray
    trigrams = tri.zipWithIndex.map({case((wrd, wt), idx) => weights(idx + unigrams.length + bigrams.length) = wt;  wrd }).toArray
    println("loaded vocab and their embeddings")
  }
}


object CreateWordEmbeddings
{
  def createEmbeddings(corpus : String) {
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
}

object WordVectorsSerialManager{

  def vectorTxt2Serial(vectorTxtLocation : String, outputSerialLocation : String) {
    val wordEmbed = new WordVectors
    wordEmbed.loadFromTxt(vectorTxtLocation)
    serialize(outputSerialLocation, wordEmbed)
  }

  def serialize(location: String, wordEmbedding: WordVectors) {
    print(s"Serializing object to location : $location ... ")
    val fos = new FileOutputStream(location)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(wordEmbedding)
    oos.close()
    println("done.")
  }

  def deserialize(location: String): WordVectors = {
    print(s"Deserializing object from location : $location ... ")
    val fis = new FileInputStream(location)
    val ois = new ObjectInputStreamWithCustomClassLoader(fis)
    val embeddings = ois.readObject.asInstanceOf[WordVectors]
    println("done.")
    embeddings
  }

  class ObjectInputStreamWithCustomClassLoader(fileInputStream: FileInputStream) extends ObjectInputStream(fileInputStream) {
    override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
      try {
        Class.forName(desc.getName, false, getClass.getClassLoader)
      }
      catch {
        case ex: ClassNotFoundException => super.resolveClass(desc)
      }
    }
  }
}