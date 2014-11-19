package rahul.NeuralNetwork
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

sealed trait LayerType

case class Input() extends LayerType
case class Output() extends LayerType
case class Hidden() extends LayerType
case class Visible() extends LayerType

trait Layer

case class ANNLayer(val numNeurons: Int, val numInp: Int = -1, val layerType: LayerType = Hidden(), val layerIdx: Int = 0) extends Layer {
  val bias = DenseVector.ones[Double](numNeurons)
  def setBiasVal(idx: Int, value: Double) = {
    bias(idx) = value
  }
}

case class RBMLayer(val numNeurons: Int, val layerType: LayerType, val bias: DenseVector[Double]) extends Layer {
  def setBiasVal(idx: Int, value: Double) = {
    bias(idx) = value
  }
}