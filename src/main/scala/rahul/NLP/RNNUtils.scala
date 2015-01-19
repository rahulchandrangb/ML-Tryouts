package rahul.NLP

import rahul.NLP.datastructs.Tree
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

//delta p
class Node(val leftChild: Node,
  val label: String,
  val labelVec:DenseVector[Double],
  val name: String,
  val fectureVector: DenseVector[Double],
  val rightChild: Node,
  val score: Double) {

  def getName: String = name
  def getFeatureVector: DenseVector[Double] = fectureVector
  def getScore: Double = score
  def setFeatureVector(vec: DenseVector[Double]) {
    0 until vec.size map {
      i =>
        this.fectureVector(i) = vec(i)
    }
  }

  def setPrecision(value: Double): Double = {
    BigDecimal(value).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def isLeaf = ((leftChild == null) && (rightChild == null))
}

object RNNConsts {
  private val labelMapStr = """
CC Coordinating conjunction
CD Cardinal number
DT Determiner
EX Existential there
FW Foreign word
IN Preposition or subordinating conjunction
JJ Adjective
JJR Adjective, comparative
JJS Adjective, superlative
LS List item marker
MD Modal
NN Noun, singular or mass
NNS Noun, plural
NNP Proper noun, singular
NNPS Proper noun, plural
PDT Predeterminer
POS Possessive ending
PRP Personal pronoun
PRP$ Possessive pronoun
RB Adverb
RBR Adverb, comparative
RBS Adverb, superlative
RP Particle
S Sentence
SYM Symbol
TO to
UH Interjection
VB Verb, base form
VBD Verb, past tense
VBG Verb, gerund or present participle
VBN Verb, past participle
VBP Verb, non­3rd person singular present
VBZ Verb, 3rd person singular present
WDT Wh­determiner
WP Wh­pronoun
WP$ Possessive wh­pronoun
WRB Wh­adverb​
"""
  private val labelMap = labelMapStr.split("\n")
    .filter(_.trim != "")
    .map {
      x =>
        val arr = x.trim.split(" ")
        (arr(0), arr.tail.mkString(" ").trim)
    }.zipWithIndex
  val labelMapWithVec: Map[String, (String, DenseVector[Double])] = labelMap.map {
    x =>
      val id = x._1._1
      val desc = x._1._2
      val classVec = DenseVector.zeros[Double](labelMap.size)
      classVec(labelMap.size - x._2 - 1) = 1.0 //Class map in reverse
      (id, (desc, classVec))
  }.toMap
  
}

object RNNUtils {

  //1.Derivative of cost function w.r.t weights

  def calcBTSLabelError(n: Node, wLabel: DenseMatrix[Double],wANN:DenseMatrix[Double],derFunc: Double => Double, labelP: DenseVector[Double], targetP: DenseVector[Double], delC:Option[(DenseVector[Double])]=None, delW:List[DenseMatrix[Double]]) = {
    //1. Calculate delta p
    val deltaP = (wLabel.t * ((labelP - targetP).toDenseMatrix)) :* (n.fectureVector.map(derFunc(_)).toDenseMatrix)   
    val deltot = delC match {
      case Some(vec:DenseVector[Double]) =>
      	   deltaP + vec.toDenseMatrix
        
      case None =>
           deltaP
    }
    
    if(n.isLeaf){
      
    }
    
    val c1c2Comb = new DenseVector((n.leftChild.fectureVector.toArray ++ n.rightChild.fectureVector.toArray))
    val deltaPDown = (wANN.t * deltaP) :* (c1c2Comb.map(derFunc(_)).toDenseMatrix)
    val delWeight = deltaP * c1c2Comb.toDenseMatrix.t
    
    
  }

}