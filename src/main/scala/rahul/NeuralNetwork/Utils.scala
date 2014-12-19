package rahul.NeuralNetwork
import scala.math._

// 1. Activation methods
object Activations {
  def sigmoid(x: Double) = {
    1 / (1 + exp(-1 * x))
  }
  def derivSigmoid(x: Double) ={
    sigmoid(x) * (1-sigmoid(x))
  }
  
  def tanH(x:Double)={
    (exp(x)-exp(-x))/(exp(x)+exp(-x))
  }  
}


