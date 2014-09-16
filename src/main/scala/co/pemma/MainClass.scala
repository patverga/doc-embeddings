package main.scala.co.pemma

import java.util

import cc.factorie.app.nlp
import cc.factorie.app.nlp.embeddings.{EmbeddingDistance, EmbeddingOpts, SkipGramNegSamplingEmbeddingModel}
import cc.factorie.la.DenseTensor1
import weka.clusterers.SimpleKMeans
import weka.core.{Attribute, DenseInstance, Instances}

import scala.collection.JavaConversions._


/**
 * Created by pv on 8/28/14.
 */
object MainClass  extends  App{

  val inputDir = "./data/clean"
  val corpus = "./data/corpus"
  val tokenizedDir = "./data/tokenized"
  val vectorFile = "./vectors"

  val numClusters = 35
  val numIterations = 250
  
  //  tokenizeText()
  //  createEmbeddings()
//    interactiveDistance()
    documentSummaries(
    "software piracy companies computer dollars us cent bsa industry pounds lost europe alliance programs copyright illegal law company market business european trade copying personal world copies uk users microsoft says news legal illegally group italy computers sales pc countries losses cost products copied rate information germany new sold publishers theft time directive number protection anti pirates lotus problem fast estimated laws court 1992 action rewards loss western network association counsel major taiwan total successful government people published management suppliers 200m vat case hardware director copy small counterfeit two pirated prosecutions office spain actions package networks 6bn based distributors system bulletin conservatively smith figures computing leading costs legislation worlds board user made big example operations product systems france manufacturers pcs unauthorised efforts spanish ec cox bsas help costing 500 found compared protect organisation country staff work rates houses applications comparing 50 electronic wide represents offer consumption development corporate typically call legitimate led local support america international highest latin viruses program claims crackdown american disks take comes general telephone german financial foreign growing control widespread years half police seized chinese put federation end drive issue department effort governments west writes president give property average month worldwide believe part decline al technology 100 premises six individual services crimeline 13 problems china operation operating pay estimate indication avoidance 737m 940608 least 133m 08 retailers cane alan 436m intellectual thailand calculated holleyman tatung late look search recently employees came aena orders enter lower designed organisations raided say evidence survey poland implementation evan spreadsheet popular licensing make spa executives offering informers payments introduction 66 hotline showed implement hit 12bn large representing running outlaw run 3bn practice suspected suspect 2bn know failed versions prepared calls level latest revenues rights 20 03 tougher cases courts audit union discs groups steps offices going subsidiary better infringement vietnam individuals result word took dropped 86 eu black corp atari filed buy represent hard bbs berlin 26 key investigation nearly past makes dongles disk site customers change consumer arab 46 line unauthorized move authors bought involved principally process institute licences place seminars owned retail priority find novell penalties free aware enforcement australia offenders containing overall serial yesterday buying crack 60 members robert 25 works 10bn common methods consumers fines apple formed increase 5bn businesses trouble city investigated portugueses uniao bancos improvement increased 200 allege increasing programmes announced public regulations cheap campaign licence departments brought single disc high rampant aldus face long strong records speaking taken target dont microsofts numbers 99 britain united sharply emirates pakistan figure hills months books form manufacturer dedicated floppy 489m specifically wording 0800 done reports reason protecting national genuine ordered particular provides companys value reasons activities send leads usually available benefits policing phone away way sell practices apparently revenue actually selling spot rules back changes expected conference deputy hwa isia think having reckoned bradford marketing manufacturing cause commission carried communications deverill barclays commercial making data indictment encouraging hopes equal issues machine ministries patent date launched central 150 largest south representative political 49 300 four prices 1bn concern equipment finance asked senior resolve compelling wholesale collected experienced units expressed 4bn quoted 920107 46bn presenting fraction explains 07 effective given 11 avoiding recent dozen machines resulted investigators legally ensure aggressively state establish special believes training passed able criminal stand join load accessed complaints tip service engineering reverse comprises order manager effect growth standard deal investment measures series list money seeking reform british impact consumed 941012 shop plague french continues totalled scandals reforming indonesia morals thieves austria 333m counter attributes tightening parts corruption wake tiny lax league north 88 bid switzerland abiding tohave asia means distribution home desktop percentage fell statistics flood annually 1991 infringers 930603 85 brad greatest 76 awareness constitutes declining campaigns operate lines focus vice handed reward malicious copyrights advanced seize area files maidenhead proceedings avoid contact 27 joanne fendley trap 931026 basingstoke arranged freefone 581062 southampton nutshell held former managing existing managers wordperfect set enthusiasts field saudi investments bloor lamacchia lamacchias team exchange prison directors access regional fastest mexico brazil chemical easy vigorously equalling campaigning confiscate securing close counterfeiters legitimately ordinance technical experts develop requires prevent authority lack demand ways produce virus watch notes london obtained proposed combat allow implementing potential importance realise convention nations ready firm particularly variety comply offered doubt commitment top sector didnt 70 uks message executive known benefit cross consist multi boards obtain 8bn 921203 cottage border distributing primary dial messages merchants establishment unsuspecting act hand feel portable gates chance bureau hidden contract bill federal pirate domestic scale wanted code producers proprietary committee probably indicted 22 increasingly areas 16 tell serious alami dughaythir aims progress met indicated enforce incontrol policy activity accounting element wants text incentive noted member goes decided employee tried install start games processing things londons earlier observance advantages pirating documentation promote education plus sale hundred identify useful quality simple fraud easier 5m eliminating associates centred passage l400bn raids portuguese illegality airports warsaw eight shops 166 diskettes javier milan owners ribas identical suspicion 330m 940513 representations spots europes autodesk anonymous markets amending portugal test needed leave secure lawyers chinas official mofert good open attention packages claimed interested developers report bad missing produced later current 89 especially draft security ecs penetration pervasive carla louise 920225 kehoe address priorities solve achieving culture petitioned section unification contends inexcusable obtaining economy 301 deeper imposition san curtail complaint 721m stems stopping rapid 08bn korea 9bn francisco sanctions east cites seek purchasing adequate easily incurred employers steel intended collects largely pace efta siemens renfe outlawing railroad industries ambiguity confusion fears dargaud ams 20bn maison force docks editeur geographie 921210 37 resolved lead la falck understand jail heavy altogether right added five considering telling additional expensive application signed monitoring stop jaman thinks 80 solution previous protected competitors parliament brussels sees extra checking dos three lin sponsor saying lynn encourage sentencing rewarded ministrys 886 intensify hailed chosen patterned tsai settlement fu healthy sound yi bfn inform cna louis innovative taipei yc 79 spokesman advocate hu launch elaborated crackdowns tzu yin aiming nt arthur secretary pascoe residents wang helping 31 " +
      "software piracy copyright infringement pirated pirate pirates copying unauthorized work countries years test online computer laws game copy control price states law united illegal video case us term time distributed amiga found windows china rate developing counterfeit bsa varies dmca gpl act protection existing illegally theft games copyrighted access public data market website instance africa websites material addition set microsoft willful rates helped apply losses heavily alleged nulled edition anticircumvention studies theres technological universally licensing xp idc romania afc stated installed rely bootleg infringing violation warez content users holders different distribution free systems generally people court system country based download " +
      "europe microsoft bsa pounds the business software alliance the bsa dollars pc italy european" +
      "Business_Software_Alliance Europe Microsoft Personal_computer China Pound_sterling Ft Dollar @UK East_Germany"
    )
//  documentSummaries(DocReader.readRobust("/home/pat/eqfe/out/docs/FT921-3046"))

  def documentSummaries(doc :String)
  {
    EmbeddingDistance.load(vectorFile)

    // split the doc into words
    val words = doc.split("\\s+").filter(w => !nlp.lexicon.StopWords.containsWord(w)  && w.size > 1)
    // turn each word into an embedding vector
    print("Converting fac vectors to weka instances...")
    val wordTensors = for ( w <- words ; wv = EmbeddingDistance.word2Vector(w); if wv != null)
    yield (w, EmbeddingDistance.word2Vector(w))

    val kmeans =clusterWords( doc, wordTensors)
    closestCentroid(kmeans, wordTensors, "software piracy")
//    sumWords(doc, wordTensors)

    println(doc)
  }

  def sumWords(doc: String, wordTensors: Array[(String, DenseTensor1)])
  {
    val embedding_in = new DenseTensor1(wordTensors.head._2.size, 0)
    wordTensors.foreach(word => embedding_in.+=(word._2))
    embedding_in./=(wordTensors.size)
    val words = EmbeddingDistance.nearestTensors(embedding_in, numClusters).map(_._1).mkString(", ")
    println("SUM WORDS")
    println(words)
  }

  def clusterWords(doc: String, wordTensors: Array[(String, DenseTensor1)]) : SimpleKMeans =
  {
    val vecSize = wordTensors.head._2.size
    val attributeList = new util.ArrayList[Attribute](vecSize + 1)
    for (i <- 0 to vecSize - 1)
      attributeList.add(new Attribute(i + ""))

    val instances = new Instances("", attributeList, wordTensors.size)
    wordTensors.foreach(t => {
      instances.add(new DenseInstance(1, t._2.toArray))
    })
    println(" Done.")

    // run kmeans with the weka
    println("Running KMeans")
    val kmeans = new SimpleKMeans()
    kmeans.setPreserveInstancesOrder(true);
    kmeans.setNumClusters(numClusters)
    kmeans.setMaxIterations(numIterations)
    kmeans.buildClusterer(instances)

    kmeans
  }

  def clusterCentroidsToWords(kmeans : SimpleKMeans)
  {
    val words = kmeans.getClusterCentroids.flatMap(centroid => {
      val tensor = new DenseTensor1(centroid.toDoubleArray)
      val centroidWords = EmbeddingDistance.nearestTensors(tensor, 1)
      centroidWords.map(_._1)
    }).mkString(", ")
    println ("CLUSTER WORDS")
    println(words)
  }

  def closestCentroid(kmeans : SimpleKMeans, words: Array[(String, DenseTensor1)], query : String)
  {
    val queryTensor = new DenseTensor1(words.head._2.size, 0)
    var queryWordsUsed = 0
    query.split("\\s+").foreach(word => {
      val wordTensor = EmbeddingDistance.word2Vector(word)
      if (wordTensor != null) {
        queryTensor.+=(wordTensor)
        queryWordsUsed += 1
      }
    })
    queryTensor./=(queryWordsUsed)

    val clusterSimilarities = for(i <- 0 to kmeans.numberOfClusters()-1) yield
    {
      val centroid = kmeans.getClusterCentroids.get(i)
      val centroidTensor = new DenseTensor1(centroid.toDoubleArray)
      (centroidTensor.cosineSimilarity(queryTensor), i)
    }
    val mostSimilarCluster = clusterSimilarities.max
    val assignments = kmeans.getAssignments
    val clusterWords = for (i <- 0 to words.size-1 if assignments(i) == mostSimilarCluster._2)
    yield words(i)._1

    clusterWords.foreach(w => println(w))
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
    EmbeddingDistance.nearestNeighbours(vectorFile, 5)
  }

}
