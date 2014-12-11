package rahul.NLP

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

case class RNNMatrix(val wScore: DenseMatrix[Double], val wLabel: DenseMatrix[Double], val bias: DenseVector[Double], val wNN: DenseMatrix[Double])
object RNNMatrix {
  def initMatrix(featureSize: Int, wordSize: Int, nnLayerSize: Int) = {
    val wScore = DenseMatrix.rand(1, featureSize)
    val wLabel = DenseMatrix.rand(wordSize, featureSize)
    val bias = DenseVector.ones[Double](nnLayerSize)
    

  }

}