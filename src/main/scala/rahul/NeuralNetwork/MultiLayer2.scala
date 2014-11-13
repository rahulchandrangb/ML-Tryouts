package rahul.NeuralNetwork
import Activations._
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.beans.BeanProperty
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.annotation.tailrec

sealed trait LayerType

case class Input extends LayerType
case class Output extends LayerType
case class Hidden extends LayerType

class MultiLayerNN(val layers: List[Layer]) {

  val weightMatrices = ListBuffer[DenseMatrix[Double]]()
  DenseMatrix.zeros[Double](
    layers.size,
    layers.foldLeft(0)((a, b) => a + b.numNeurons))
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

  def calculateOutput = forward(0, input)
  @tailrec
  private def forward(layernum: Int = 0, curInputs: DenseVector[Double] = input): DenseVector[Double] = {
    if (layernum == layers.size) curInputs
    val newInp = getActivation(layernum, curInputs)
    forward(layernum + 1, newInp)

  }
}

object MultiLayerNN {
  def loadModelFromFile(srcFile: String): MultiLayerNN = {
    val layerList = Source.fromFile(srcFile).getLines.toList.map( x => Layer(x.toInt))
    new MultiLayerNN(layerList)
  }
}

case class Layer(val numNeurons: Int) {
  val bias = DenseVector.zeros[Double](numNeurons)
}