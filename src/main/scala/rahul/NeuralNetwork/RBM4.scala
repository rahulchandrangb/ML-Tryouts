package rahul.NeuralNetwork
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

class RBM(
  val numVisible:Int,
  val numHidden:Int,
  val weight:DenseMatrix[Double],
  val visibleLayer:RBMLayer,
  val hiddenLayer:RBMLayer
) {

  def calcCD {

  }

}
object RBM {

}