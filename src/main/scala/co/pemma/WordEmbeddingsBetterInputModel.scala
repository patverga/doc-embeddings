//package co.pemma
//
//import java.io.FileInputStream
//import java.util.zip.GZIPInputStream
//
//import cc.factorie.app.nlp.embeddings.{VocabBuilder, SkipGramNegSamplingEmbeddingModel}
//
///**
// * Created by pv on 8/28/14.
// */
//class WordEmbeddingsBetterInputModel extends  SkipGramNegSamplingEmbeddingModel
//{
//  // Component-1
//
//  override def buildVocab(): Unit = {
//    vocab = new VocabBuilder(vocabHashSize, samplingTableSize, 0.7) // 0.7 is the load factor
//    if (loadVocabFilename.size == 0) {
//      println("Building Vocab")
//      val corpusLineItr = corpus.endsWith(".gz") match {
//        case true => io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(corpus)), encoding).getLines
//        case false => io.Source.fromInputStream(new FileInputStream(corpus), encoding).getLines
//      }
//      while (corpusLineItr.hasNext) {
//        val line = corpusLineItr.next
//        line.stripLineEnd.split(' ').foreach(word => vocab.addWordToVocab(word)) // addWordToVocab() will incr by count. TODO : make this also parallel ? but it is an one time process, next time use load-vocab option
//      }
//    } else {
//      println("Loading Vocab")
//      vocab.loadVocab(loadVocabFilename, encoding)
//    }
//    vocab.sortVocab(minCount, ignoreStopWords, maxVocabSize) // removes words whose count is less than minCount and sorts by frequency
//    vocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
//    vocab.buildSubSamplingTable(opts.sample.value) // precompute subsampling table
//    V = vocab.size()
//    train_words = vocab.trainWords()
//    println("Corpus Stat - Vocab Size :" + V + " Total words (effective) in corpus : " + train_words)
//    // save the vocab if the user provides the filename save-vocab
//    if (saveVocabFilename.size != 0) {
//      println("Saving Vocab into " + saveVocabFilename)
//      vocab.saveVocab(saveVocabFilename, storeInBinary, encoding) // for every word, <word><space><count><newline>
//      println("Done Saving Vocab")
//    }
//
//  }
//
//}
