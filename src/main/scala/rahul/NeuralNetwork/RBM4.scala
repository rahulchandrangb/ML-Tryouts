package rahul.NeuralNetwork
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Binomial
import breeze.stats.distributions.Uniform

class RBM(val visibleLayer: RBMLayer,
  val hiddenLayer: RBMLayer,
  val weight: DenseMatrix[Double],
  val boolRandWeights: Boolean = false) {

  def this(visibleLayer: RBMLayer, hiddenLayer: RBMLayer) = {
    this(visibleLayer, hiddenLayer, DenseMatrix.zeros[Double](visibleLayer.numNeurons, hiddenLayer.numNeurons), true)
  }

  val numVisible = visibleLayer.numNeurons
  val numHidden = hiddenLayer.numNeurons
  if (boolRandWeights) initWeights

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

  private def calcActivationEnergy(input: DenseVector[Double],weight:DenseMatrix[Double] ,boolVtoH: Boolean = true) = {
    if (boolVtoH) (weight * input.toDenseMatrix.t).toDenseVector
    else (weight.t * input.toDenseMatrix.t).toDenseVector
  }

  def calcActivationAndSamp(layerType:LayerType,input:DenseVector[Int])={
    
  }
  def contrastiveDiv(
    input: DenseVector[Int],
    learningRate: Double, numIterations: Int = 2,
    numHidden: Int = this.numHidden,
    numVisible: Int = this.numVisible) {

    val posHidMean = DenseVector.zeros[Double](numHidden)
    val posHidSample = DenseVector.zeros[Double](numHidden)
  
    
    val negVisMean = DenseVector.zeros[Double](numVisible)
    val negVisSample = DenseVector.zeros[Double](numVisible)
    val negHidMean = DenseVector.zeros[Double](numHidden)
    val negHidSample = DenseVector.zeros[Double](numHidden)

  }

}
object RBM {

}