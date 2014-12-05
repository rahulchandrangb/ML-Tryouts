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

// 2. Strategy
// 2.1 Training strategy
trait TrainStrategy
case class TrainByEpoch(val numEpoch:Int) extends TrainStrategy
case class TrainByError(val errorThresholds:Double) extends TrainStrategy

// RNN parent selection strategy.
trait PSelectStrategy
case class GreedySelect() extends PSelectStrategy
case class BruteforceBest() extends  PSelectStrategy