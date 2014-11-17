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

class MultiLayerNN(val layers: List[Layer], val weightMatrices: ListBuffer[DenseMatrix[Double]] = ListBuffer[DenseMatrix[Double]](), val learningRate: Double = 0.2, val momentum: Double = 0.3) {
  val activatedInputList = ListBuffer[DenseVector[Double]]()

  val inpVecSize = layers.size
  (0 until inpVecSize).map {
    x =>
      activatedInputList += DenseVector()
  }

  def initWeightMatrix { // Initialize with a Uniform Distribution

  }
  val input = Data.SAMPLE_INPUT

  def getActivation(layernum: Int, curInputs: DenseVector[Double]): DenseVector[Double] = {
    val weightMatOfLayer = weightMatrices(layernum)
    val nextLayerNeuronCnt =
      if (layernum < layers.size - 1)
        layers(layernum + 1).numNeurons
      else
        1
    val activationVect = DenseVector.zeros[Double](nextLayerNeuronCnt)
    for (i <- 0 until nextLayerNeuronCnt) {
      activationVect(i) = sigmoid(
        curInputs.toArray.zipWithIndex
          .map(x => weightMatOfLayer(i, x._2) * x._1)
          .reduce(_ + _) + layers(layernum).bias(i))
    }
    activationVect
  }

  def computeSquaredError(target:List[Double],computedOut:List[Double]):Double = {
    (target.zip(computedOut).map(x => pow((x._1-x._2),2)).reduce(_+_))/2
  }
  
  def calculateOutput = forward(0, input)

  @tailrec
  private def forward(layernum: Int = 0, curInputs: DenseVector[Double] = input): DenseVector[Double] = {
    if (layernum == layers.size) curInputs
    else {
      val newInp = getActivation(layernum, curInputs)
      activatedInputList(layernum) = newInp //Side effect !!!!
      forward(layernum + 1, newInp)
    }
  }

  private def findDelta(layer: Layer) = {
    //Check whether it's output layer
    val delta = layer.layerType match {
      case Output() =>
      	
      case _ =>

    }
  }
}

object MultiLayerNN {
  def loadModelFromFile(srcFile: String): MultiLayerNN = {
    val inputList: List[(String, Int)] = Source.fromFile(srcFile).getLines.zipWithIndex.toList
    val lastElemIdx = inputList.size - 1
    val layerList: List[Layer] = inputList.map {
      x =>
        if (x._2 == 0) { //First element?  get input, set LayerTYpe as Input
          val lyrDtSplit = x._1.split(",")
          val numNeurons = lyrDtSplit(0).toInt
          val numInp = lyrDtSplit(1).toInt
          Layer(numNeurons, numInp, Input())
        } else if (x._2 == lastElemIdx) { // Last element..So Output layer
          Layer(x._1.toInt, layerType = Output())
        } else { // Hidden Layer
          Layer(x._1.toInt)
        }
    }
    new MultiLayerNN(layerList)
  }
}

case class Layer(val numNeurons: Int, val numInp: Int = -1, val layerType: LayerType = Hidden()) {
  val bias = DenseVector.zeros[Double](numNeurons)
}