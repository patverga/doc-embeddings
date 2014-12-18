package co.pemma.embeddings

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import cc.factorie.app.nlp.embeddings.{SkipGramNegSamplingEmbeddingModel, EmbeddingOpts}
import cc.factorie.la.{DenseTensor2, DenseTensor1}


import collection.mutable
import collection.mutable._
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.JavaConversions.mapAsScalaMap

/**
 * Created by pat on 9/26/14.
 */
class WordVectors extends Serializable
{
  var threshold = 0
  var unigrams = mutable.Map[String, Int]()
  var bigrams = mutable.Map[String, Int]()
  var trigrams = mutable.Map[String, Int]()
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
    unigrams = new java.util.HashMap[String, Int]()
    uni.zipWithIndex.foreach({case((wrd, wt), idx) => weights(idx) = wt; unigrams.put(wrd, idx) })
    bigrams = new java.util.HashMap[String, Int]()
    bi.zipWithIndex.map({case((wrd, wt), idx) => val i = idx+unigrams.size; weights(i) = wt;  bigrams.put(wrd,i) }).toArray
    trigrams = new java.util.HashMap[String, Int]()
    tri.zipWithIndex.map({case((wrd, wt), idx) => val i = idx+unigrams.size+bigrams.size; weights(i) = wt;  trigrams.put(wrd,i) })
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
    serializeWordVectors(outputSerialLocation, wordEmbed)
  }

  def serializeWordVectors(location: String, wordEmbedding: WordVectors) {
    print(s"Serializing object to location : $location ... ")
    val fos = new FileOutputStream(location)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(wordEmbedding)
    oos.close()
    println("done.")
  }

  def deserializeWordVectors(location: String): WordVectors = {
    print(s"Deserializing object from location : $location ... ")
    val fis = new FileInputStream(location)
    val ois = new ObjectInputStreamWithCustomClassLoader(fis)
    val embeddings = ois.readObject.asInstanceOf[WordVectors]
    println("done.")
    embeddings
  }

  def serializeTransformationMatrix(location: String, matrix: DenseTensor2) {
    print(s"Serializing object to location : $location ... ")
    val fos = new FileOutputStream(location)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(matrix)
    oos.close()
    println("done.")
  }

  def deserializeTransformationMatrix(location: String): DenseTensor2 = {
    print(s"Deserializing object from location : $location ... ")
    val fis = new FileInputStream(location)
    val ois = new ObjectInputStreamWithCustomClassLoader(fis)
    val matrix = ois.readObject.asInstanceOf[DenseTensor2]
    println("done.")
    matrix
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

object SerializeVectorTxt extends App {
  assert(args.length > 0, "Must supply an input vector file.")
  val inLocation = args(0)
  val outLocation = inLocation + ".dat"

  WordVectorsSerialManager.vectorTxt2Serial(inLocation, outLocation)
  //  println(distance.phrase2Vec("bill clinton"))
  if (args.length > 1) {
    val distance = new WordVectorMath(WordVectorsSerialManager.deserializeWordVectors(outLocation))
    distance.interactiveNearestNeighbor()
  }
}