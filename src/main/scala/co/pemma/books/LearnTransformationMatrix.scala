package co.pemma.books

import cc.factorie.app.nlp.embeddings.{WordEmbeddingModel, LiteHogwildTrainer, TensorUtils}
import cc.factorie.la.{DenseTensor2, DenseTensor1, WeightsMapAccumulator}
import cc.factorie.model.Parameters
import cc.factorie.optimize.{Example, AdaGradRDA}
import cc.factorie.util.DoubleAccumulator
import co.pemma.embeddings.{WordVectorsSerialManager, WordVectorMath}

/**
 * Created by pv on 10/20/14.
 */
class TransformationMatrixModel extends Parameters
{
  val vectorLocation = "./vectors/newswire-vectors.dat"
  val wordVecs = new WordVectorMath(WordVectorsSerialManager.deserialize(vectorLocation))
  val D = wordVecs.D

  val adaGradDelta = 0.1
  val adaGradRate = 0.025
  val optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
  val weights = Weights(TensorUtils.setToRandom2(new DenseTensor2(D, D, 0))) // initialized using wordvec random
  optimizer.initializeWeights(this.parameters)
  val trainer = new LiteHogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, maxIterations = Int.MaxValue)

  def train() {
    val trainingPairs = Seq(("cat", "pumpkin"), ("dog", "macaroni"), ("De-Dazzler", "mashed"))

    for (i <- 1 to 100) {
      trainingPairs.foreach { case (a, b) => trainer.processExample(new TransformationExample(this, wordVecs.word2Vec(a), wordVecs.word2Vec(b)))
      }
    }

    trainingPairs.foreach { case (a, b) =>
      println (s"-----$a")
      wordVecs.nearestNeighbors(Array(a), wordVecs.word2Vec(a), 10).foreach(println(_))
      println(s"-----$b")
      wordVecs.nearestNeighbors(Array(b), wordVecs.word2Vec(b), 10).foreach(println(_))
      println(s"-----${b}2$a")
      wordVecs.nearestNeighbors(Array(), new DenseTensor1(weights.value.leftMultiply(wordVecs.word2Vec(b))), 10).foreach(println(_))
    }
  }

}

class TransformationExample(model:TransformationMatrixModel, v1: DenseTensor1, v2: DenseTensor1) extends Example {

  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit =
  {
    val v1Transformed = model.weights.value.leftMultiply(v1)
    val distance = 1.0 - v1Transformed.cosineSimilarity(v2)

    value.accumulate(distance)
    gradient.accumulate(model.weights, v1, distance)
  }
}

object TestTransform extends App{
  val transform = new TransformationMatrixModel
  transform.train()
}
