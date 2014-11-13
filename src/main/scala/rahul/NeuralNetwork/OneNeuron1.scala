package rahul.NeuralNetwork
import scala.math._
import breeze.linalg.DenseVector
import Activations._

object Neuron {
  val numInps = 2
  val weights: DenseVector[Double] = DenseVector.ones(numInps+1)
  val inps = List(List(0.0, 0.0), List(0.0, 1.0), List(1.0, 0.0), List(1.0, 1.0))
  val bias = 0.5
  val outs = List(0, 0, 0, 1.0)

  
  def calculateOutput(i: Int = 0, j: Int = 0, sum: Double = 0.0): Double = {
    if (i >= numInps) sum+weights(i)*bias
    else calculateOutput(i + 1, j, sum + weights(i) * inps(j)(i))
  }
  
  def main(args:Array[String])={
    println("Sigmoid for each inp")
    val kv = for{
      i <- 0 until inps.size
    }yield(i,calculateOutput(j=i))
    kv.foreach(x => println(inps(x._1).mkString(",")+ " : "+x._2+" : "+sigmoid(x._2) +"  :  "+tanH(x._2)))
  }

}