package rahul.NeuralNetwork
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Binomial
import breeze.stats.distributions.Uniform

class RBM(val visibleLayer: RBMLayer,
  val hiddenLayer: RBMLayer,
  val weight: DenseMatrix[Double]) {

  def this(visibleLayer: RBMLayer, hiddenLayer: RBMLayer) = {
    this(visibleLayer, hiddenLayer, DenseMatrix.zeros[Double](visibleLayer.numNeurons, hiddenLayer.numNeurons))
  }

  val numVisible = visibleLayer.numNeurons
  val numHidden = hiddenLayer.numNeurons

  def initWeights { //Initialize with normal distribution with mean 0.0, and standard deviation 0.1
    val norm = new Gaussian(0.0, 0.1)
    val cartesian = for {
      i <- 0 until weight.rows
      j <- 0 until weight.cols
    } yield (i, j)
    cartesian.foreach {
      case (i, j) =>
        weight(i, j) = norm.draw
    }
  }

  private def computeActivationEnergy(unitIdx:Int,layer:Layer,input:DenseVector[Double])={
     weight * input.toDenseMatrix.t
  }
  
  
  def calcCD {

  }

}
object RBM {

}