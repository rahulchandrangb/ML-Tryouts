package rahul.NLP

import rahul.NLP.datastructs.Tree
import breeze.linalg.{DenseMatrix,DenseVector}
import rahul.NeuralNetwork.LabelPredict

//delta p
class Node(val leftChild: Node,
  val label: String,
  val labelVec: DenseVector[Double],
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

  /*
   * Calculates Back Propagation Through Structure -> Label Prediction deltas.
   * 
   * Output : (List(delta W), List(deltaWLabel))
   */
  
  def calcBTSLabelError(n: Node,
      wLabel:DenseMatrix[Double],
      wANN: DenseMatrix[Double], 
      derFunc: Double => Double, 
      delC: Option[(DenseVector[Double])] = None, 
      delW: List[DenseMatrix[Double]] = List(),
      delWLabel:List[DenseMatrix[Double]]=List()): (List[DenseMatrix[Double]],List[DenseMatrix[Double]]) = { 
    //1. Calculate delta p
    
    val labelP = n.labelVec
    val targetP = RNNConsts.labelMapWithVec(n.label)._2
    val deltaWLabel = (targetP-labelP) * n.fectureVector.toDenseMatrix.t
    val deltaP = (wLabel.t * ((labelP - targetP).toDenseMatrix)) :* (n.fectureVector.map(derFunc(_)).toDenseMatrix)
    val deltot = delC match {
      case Some(vec: DenseVector[Double]) =>
        deltaP + vec.toDenseMatrix
      case None =>
        deltaP
    }

    if (n.isLeaf) {
      (delW,delWLabel :+ deltaWLabel)
    } else {
      val c1c2Comb = new DenseVector((n.leftChild.fectureVector.toArray ++ n.rightChild.fectureVector.toArray))
      val deltaPDown = (wANN.t * deltot) :* (c1c2Comb.map(derFunc(_)).toDenseMatrix)
      val (deltaC1Data,deltaC2Data) = deltaPDown.data.splitAt(deltaPDown.data.size/2)
      val (delC1Down,delC2Down) = (new DenseVector(deltaC1Data),new DenseVector(deltaC2Data))      
      val delWeight = deltot * c1c2Comb.toDenseMatrix.t
      val (newDelWeights,newDelWLabels) = calcBTSLabelError(n.leftChild,wLabel,wANN,derFunc,Some(delC1Down),delW :+ delWeight,delWLabel :+ deltaWLabel)
      calcBTSLabelError(n.rightChild,wLabel,wANN,derFunc,Some(delC2Down),newDelWeights,newDelWLabels)
    }
  }

}