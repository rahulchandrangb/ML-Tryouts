package rahul.NeuralNetwork
import breeze.linalg.DenseVector
trait Network{
  
  def initialize
  def train(strategy:TrainStrategy)
  def setInpOutp(inp:DenseVector[Double],out:DenseVector[Double])  
  def predict(inp:DenseVector[Double]):DenseVector[Double]
  
  
}