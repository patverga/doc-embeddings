package co.pemma.books

import cc.factorie.app.nlp.embeddings.{LiteHogwildTrainer, TensorUtils}
import cc.factorie.la.{DenseTensor2, DenseTensor1, WeightsMapAccumulator}
import cc.factorie.model.Parameters
import cc.factorie.optimize._
import cc.factorie.util.DoubleAccumulator
import co.pemma.embeddings.{WordVectorsSerialManager, WordVectorUtils}

import scala.io.Source
/**
 * Created by pv on 10/20/14.
 */
class TransformationMatrixModel(t1Location : String, t2Location : String, output : String) extends Parameters
{
  implicit val random = new scala.util.Random(0)

  val T1 = new WordVectorUtils(WordVectorsSerialManager.deserializeWordVectors(t1Location))
  val T2 = if (t1Location==t2Location || t2Location =="") T1 else
   new WordVectorUtils(WordVectorsSerialManager.deserializeWordVectors(t2Location))

  val adaGradDelta = 0.1
  val adaGradRate = 0.025
  val optimizer = new AdaGrad(delta = adaGradDelta, rate = adaGradRate)
  val weights = Weights(TensorUtils.setToRandom2(new DenseTensor2(T1.D, T2.D, 0)))
  optimizer.initializeWeights(this.parameters)

  def train() {

    val source = Source.fromURL(getClass.getResource("/function_words"))
    val functionWords = source.getLines().toList.filter(fw => T1.unigrams.contains(fw) && T2.unigrams.contains(fw))
    source.close()

    val X = new DenseTensor2(T1.D, functionWords.size)
    val Z = new DenseTensor2(T2.D, functionWords.size)
    var i = 0
    while (i < functionWords.size){

      val x = T1.word2Vec(functionWords(i))
      val z = T2.word2Vec(functionWords(i))
      var j = 0
      while (j < x.dim1) {
        X.update(i, j, x(j))
        j += 1
      }
      j = 0
      while (j < z.dim1) {
        Z.update(i, j, z(j))
        j += 1
      }
      i += 1
    }


    val examples = functionWords.map(word => {new TransformationExample(this, X, Z, T1.word2Vec(word), T2.word2Vec(word))})
    Trainer.onlineTrain(this.parameters, optimizer = optimizer, maxIterations = 10, examples = examples)

    val testWords = Seq("oil", "shuttle", "the", "and")

    testWords.foreach (tw => {
      println (s"----- $tw in T1")
      T1.nearestNeighbors(Array(tw), T1.word2Vec(tw), 5).foreach(println(_))
      println (s"----- $tw in T2")
      T2.nearestNeighbors(Array(tw), T2.word2Vec(tw), 5).foreach(println(_))
      println(s"-----$tw projected from T1 into T2")
      T2.nearestNeighbors(Array(), new DenseTensor1(weights.value.leftMultiply(T1.word2Vec(tw))), 5).foreach(println(_))
    })

    WordVectorsSerialManager.serializeTransformationMatrix(output, weights.value.asInstanceOf[DenseTensor2])
  }

}

class TransformationExample(model:TransformationMatrixModel, X : DenseTensor2, Z : DenseTensor2,
                            v1: DenseTensor1, v2: DenseTensor1) extends Example
{
  def accumulateValueAndGradient(value: DoubleAccumulator, gradientAccumulator: WeightsMapAccumulator): Unit =
  {
    val W = model.weights.value.asInstanceOf[DenseTensor2]
    val v1Transformed = W.leftMultiply(v1)
    val XW = W.dot(X)
//    val gradient = Z.-(XW).dot(v1)
    val distance = v1Transformed.l2Similarity(v2)

    value.accumulate(distance * distance)
    gradientAccumulator.accumulate(model.weights, v1)
  }
}

object TestTransform extends App{
  val transform = new TransformationMatrixModel("./books/vectors/1910-300.dat", "./books/vectors/1850-300.dat", "matrix/1910-to-1850.dat")
  transform.train()
}
