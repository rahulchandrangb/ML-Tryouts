package rahul.NLP
import breeze.linalg.{DenseMatrix,DenseVector}
/*
 * This is a mock neural network..nothing more than a sample weight and a bias, a perceptron 
 * used to help in debugging and impl RNN.for actual impl, 
 * see rahul.NeuralNetwork implementations of ffnn, rbm, ae etc.
 */
class NeuralNetForVector(val weightMatrix:DenseMatrix[Double],val bias:DenseVector[Double]){
  def calculateParentVec(leftVal:DenseVector[Double],rightVal :DenseVector[Double])={
     leftVal * weightMatrix(0,0) + rightVal * weightMatrix(0,1)
  }
}
