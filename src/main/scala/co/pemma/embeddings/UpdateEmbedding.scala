package co.pemma.embeddings

import java.io.{File, FileInputStream}
import java.util.zip.GZIPInputStream

import cc.factorie.app.nlp.embeddings._
import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.AdaGradRDA
import cc.factorie.util.Threading
import co.pemma.{ExpansionModels, HTMLTextCleaner}
import co.pemma.books.BookTimeSearcher
import co.pemma.books.TimeDriftTest._
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

/**
 * Created by pv on 1/1/15.
 */
object UpdateEmbedding extends BookTimeSearcher {

  def main(args: Array[String]) {
    val (qid: Int, query: String, subjects: Map[String, String], searcher: GalagoSearcher, cleanQuery: String, querySet: String, editDistThreshold: Double) = initialize(args)

    val wordVecStart = new WordVectorUtils(WordVectorsSerialManager.deserializeWordVectors("/home/pv/doc-embeddings/vectors/decade-vectors/180-194.vectors.dat"))

    wordVecStart.nearestNeighbors(Array("turkey"),wordVecStart.word2Vec("turkey"),20).foreach(w => println(w + " "))
//    wordVecStart.nearestNeighbors(Array("washington"),wordVecStart.word2Vec("washington"),5).foreach(w => print(w + " "))

    val sdmQuery = GalagoQueryBuilder.seqdep("turkey thanksgiving bird").queryStr
    val sdmRankings = searcher.retrieveScoredDocuments(sdmQuery, None, 5000)

    val docs = searcher.fetchDocuments(sdmRankings).map(d => HTMLTextCleaner.cleanHTML(d.doc.text))
    println(s"\n Expansion Terms")
    docs.take(20).foreach(d => println(d + "\n----------------------------------\n"))
    println()
    val words = docs.mkString("\n").split("\\s+").toSet.toSeq
//    val words = (wordVecStart.bigrams.keys ++ wordVecStart.trigrams.keys ++ wordVecStart.unigrams.keys).toSeq


    val rmExpansionTerms = ExpansionModels.lce(sdmRankings take numExpansionDocs, searcher, numExpansionTerms).
      filterNot(t => digitRegex.pattern.matcher(t._1).matches() || langRegex.pattern.matcher(t._1).matches() || cleanQuery.contains(t._1))
    println(s"\n Expansion Terms")
    rmExpansionTerms.take(20).foreach(println(_))
    println()

    val corpus = "./update-file"
    val out = "./testingupdate"
    val opts = new EmbeddingOpts()
    opts.parse(Seq(
      "--ignore-stopwords=true",
      "--threads=48",
      "--encoding=UTF8",
      "--save-vocab=./vocab",
      "--min-count=1",
      //    "--load-vocab=./vocab",
      s"--train=./$corpus",
      s"--output=./$out"))

    val wordEmbedModel = new UpdateableSkipGramEmbeddingModel(opts)
    wordEmbedModel.initializeFromModel(wordVecStart)
//    wordEmbedModel.initializeFromStrings(words)
    wordEmbedModel.updateModel(docs)


//    wordVecStart.nearestNeighbors(Array("turkey"),wordVecStart.word2Vec("turkey"), 20).foreach(w => println(w + " "))
    wordEmbedModel.distance("turkey", 20, words)

  }


}
  
class UpdateableSkipGramEmbeddingModel(override val opts: EmbeddingOpts) extends SkipGramNegSamplingEmbeddingModel(opts)
  {
  private var train_words: Long = 0 // total # of words in the corpus. Needed to calculate the distribution of the work among threads and seek points of corpus file
  val iterations = 15

  def initializeFromModel(wv : WordVectorUtils): Unit = {
    vocab = new VocabBuilder(vocabHashSize, samplingTableSize, 0.7) // 0.7 is the load factor
    val weightDex : Seq[Int] = wv.unigrams.map { case (w, i) => vocab.addWordToVocab(w); i }.toSeq ++
        wv.bigrams.map { case (w, i) => vocab.addWordToVocab(w); i }.toSeq ++
        wv.trigrams.map { case (w, i) => vocab.addWordToVocab(w); i }.toSeq

    updateVocab()

    weights = weightDex.map(i => Weights(wv.weights(i)))

  }

  def initializeFromStrings(words:Seq[String]): Unit = {
    vocab = new VocabBuilder(vocabHashSize, samplingTableSize, 0.7) // 0.7 is the load factor
    words.foreach(w=> vocab.addWordToVocab(w))
    updateVocab()
    weights = (0 until V).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random

  }

  def updateVocab() {
    vocab.sortVocab(minCount, ignoreStopWords, Int.MaxValue) // removes words whose count is less than minCount and sorts by frequency
    vocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
    vocab.buildSubSamplingTable(opts.sample.value) // precompute subsampling table
    V = vocab.size()
    train_words = vocab.trainWords()
    println("Corpus Stat - Vocab Size :" + V + " Total words (effective) in corpus : " + train_words)
  }

  def updateModel(docs : Seq[String]): Unit ={

    // add new words to vocab

    // create random weights for new words


    // train the model to update weights
    optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)

    optimizer.initializeWeights(this.parameters)
    trainer = new LiteHogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)

    for (it <- 0 to iterations) {
      println(s"Iteration $it")
      val threadIds = (0 until docs.size).map(i => i)
      Threading.parForeach(threadIds, threads)(threadId => updateThread(threadId, docs))
    }
    println("Done learning embeddings. ")
    //store()
  }

  def updateThread(id: Int, docs:Seq[String], printAfterNDoc: Long = 100): Unit = {
    val doc = docs(id)
    val lines = doc.split("\\.")
    val lineItr = lines.iterator
    var word_count: Long = 0
    var ndoc = 0

    while (lineItr.hasNext) {
      word_count += process(lineItr.next()) // Design choice : should word count be computed here and just expose process(doc : String): Unit ?.
      ndoc += 1
    }
  }

  def distance(word: String, K :Int, words:Seq[String]): Unit ={
    val embedding_in = weights(vocab.getId(word)).value

    val pq = new mutable.PriorityQueue[(String, Double)]()(dis())
    words.zipWithIndex.foreach{case (w, index) =>
      val id = vocab.getId(w)
      if (id > -1) {
        val embedding_out = weights(id).value
        val score = embedding_in.cosineSimilarity(embedding_out)
        if (index < K)
          pq.enqueue(w -> score)
        else if (score > pq.head._2) {
          // if the score is greater the min, then add to the heap
          pq.dequeue()
          pq.enqueue(w -> score)
        }
      }
    }
    val arr = new Array[(String, Double)](pq.size)
    var i = 0
    while (pq.nonEmpty) { // min heap
      arr(i) = (pq.head._1, pq.head._2)
      i += 1
      pq.dequeue()
    }
    println("\t\t\t\t\t\tWord\t\tCosine Distance")
    arr.foreach(x => println("%50s\t\t%f".format(x._1, x._2)))
  }


  // private helper functions
  private def dis() = new Ordering[(String, Double)] {
    def compare(a: (String, Double), b: (String, Double)) = -a._2.compare(b._2)
  }

}
