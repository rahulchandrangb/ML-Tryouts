package rahul.NeuralNetwork
import Activations._
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.math._
import scala.beans.BeanProperty
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.annotation.tailrec

sealed trait LayerType

case class Input extends LayerType
case class Output extends LayerType
case class Hidden extends LayerType

class MultiLayerNN(val layers: List[Layer],
  val weightMatrices: ListBuffer[DenseMatrix[Double]] = ListBuffer[DenseMatrix[Double]](),
  val learningRate: Double = 0.2, // Determines the fraction of weight and bias change
  val momentum: Double = 0.3, // To prevent local minima during GD
  val input: DenseVector[Double] = Data.SAMPLE_INPUT,
  val output: DenseVector[Double] = Data.SAMPLE_OUTPUT,
  val activationFunc: Double => Double = Activations.sigmoid,
  val derivActivationFunc: Double => Double = Activations.derivSigmoid) {
  val activatedInputList = ListBuffer[DenseVector[Double]]()
  val deltaVectorList = ListBuffer[DenseVector[Double]]()

  val layerSz = layers.size

  def initialize = {
    //1. Initialize collection vectors
    (0 until layerSz).map {
      x =>
        activatedInputList += DenseVector()
        deltaVectorList += DenseVector()
    }
    //2. Initialize weight Matrix
    initWeightMatrix
  }

  private def initWeightMatrix { // Initialize with a Uniform Distribution..??

  }

  def getActivation(layernum: Int, curInputs: DenseVector[Double]): DenseVector[Double] = {
    val weightMatOfLayer = weightMatrices(layernum)
    val nextLayerNeuronCnt =
      if (layernum < layers.size - 1)
        layers(layernum + 1).numNeurons
      else
        1
    val activationVect = DenseVector.zeros[Double](nextLayerNeuronCnt)
    for (i <- 0 until nextLayerNeuronCnt) {
      activationVect(i) = activationFunc(
        curInputs.toArray.zipWithIndex
          .map(x => weightMatOfLayer(i, x._2) * x._1)
          .reduce(_ + _) + layers(layernum).bias(i))
    }
    activationVect
  }

  def computeSquaredError(target: List[Double], computedOut: List[Double]): Double = {
    (target.zip(computedOut).map(x => pow((x._1 - x._2), 2)).reduce(_ + _)) / 2
  }

  def calculateOutput = forward(0, input)

  @tailrec
  private def forward(layerNum: Int = 0, curInputs: DenseVector[Double] = input): DenseVector[Double] = {
    if (layerNum == layers.size) curInputs
    else {
      val newInp = getActivation(layerNum, curInputs)
      activatedInputList(layerNum) = newInp //Side effect !!!!
      forward(layerNum + 1, newInp)
    }
  }
  def backPropagate {
    updateDeltaList() //Propagate and find the delta for all layers

  }

  @tailrec
  private def updateDeltaList(layer: Layer = layers.last): Unit = {
    //Check whether it's output layer  
    val deltaVector: DenseVector[Double] = layer.layerType match {
      case Output() =>
        DenseVector(activatedInputList(layer.layerIdx).toArray
          .zipWithIndex.map { x => derivActivationFunc(x._1) * (output(x._2) - x._1) })
      case _ =>
        val deltaAlone = (deltaVectorList(layer.layerIdx + 1).toDenseMatrix * weightMatrices(layer.layerIdx + 1)).toDenseVector
        val derivOutput = activatedInputList(layer.layerIdx).map(derivActivationFunc)
        deltaAlone :* derivOutput
    }
    deltaVectorList(layer.layerIdx) = deltaVector
    if (layer.layerIdx - 1 == 0) return
    else updateDeltaList(layers(layer.layerIdx - 1))
  }
}

object MultiLayerNN {
  def loadModelFromFile(srcFile: String): MultiLayerNN = {
    val inputList: List[(String, Int)] = Source.fromFile(srcFile).getLines.zipWithIndex.toList
    val lastElemIdx = inputList.size - 1
    val layerList: List[Layer] = inputList.map {
      x =>
        if (x._2 == 0) { //First element?  get input, set LayerType as Input
          val lyrDtSplit = x._1.split(",")
          val numNeurons = lyrDtSplit(0).toInt
          val numInp = lyrDtSplit(1).toInt
          Layer(numNeurons, numInp, Input())
        } else if (x._2 == lastElemIdx) { // Last element..So Output layer
          Layer(x._1.toInt, layerType = Output(), layerIdx = x._2)
        } else { // Hidden Layer
          Layer(x._1.toInt, layerIdx = x._2)
        }
    }
    new MultiLayerNN(layerList)
  }
}

case class Layer(val numNeurons: Int, val numInp: Int = -1, val layerType: LayerType = Hidden(), val layerIdx: Int = 0) {
  val bias = DenseVector.zeros[Double](numNeurons)
}