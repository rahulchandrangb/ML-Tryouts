package rahul.NeuralNetwork
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Binomial
import breeze.stats.distributions.Uniform
import scala.collection.mutable.ListBuffer

class RBM(val visibleLayer: RBMLayer,
  val hiddenLayer: RBMLayer,
  val weight: DenseMatrix[Double],
  val boolRandWeights: Boolean = false) {

  def this(visibleLayer: RBMLayer, hiddenLayer: RBMLayer) = {
    this(visibleLayer, hiddenLayer, DenseMatrix.zeros[Double](visibleLayer.numNeurons, hiddenLayer.numNeurons), true)
  }
  val numVisible = visibleLayer.numNeurons
  val numHidden = hiddenLayer.numNeurons
  val vbias = visibleLayer.bias
  val hbias = hiddenLayer.bias
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

  private def calcActivationEnergy(input: DenseVector[Double], boolVtoH: Boolean = true) = {
    if (boolVtoH) (weight * input.toDenseMatrix.t).toDenseVector
    else (weight.t * input.toDenseMatrix.t).toDenseVector //Negetive direction..
  }
  def gibbsSampling(sampleInp: DenseVector[Double]) = {
    println("Gibbs sampling")
    val (actVis, sampVis) = computeActivAndSample(visibleLayer, sampleInp)
    println(s"Hidden inp: $sampVis ")
    val (actHid, sampHid) = computeActivAndSample(hiddenLayer, sampVis)
    (sampHid, actHid, sampVis, actVis)
  }

  def computeActivAndSample(destLayer: RBMLayer, input: DenseVector[Double]) = {
    destLayer.layerType match {
      case Visible() =>
        val sampleVec = ListBuffer[Int]()
        val activationStore = DenseVector.zeros[Double](destLayer.numNeurons)
        for (i <- 0 until destLayer.numNeurons) {
          val newWeight = weight.t
          val sigmaPlusBias = (input.toDenseMatrix.t * newWeight(i, ::)).apply(0,0) + vbias(i)
          
          println(s"sigmabias $sigmaPlusBias")
          val activatedOut = Activations.sigmoid(sigmaPlusBias)
          activationStore(i) = activatedOut
          sampleVec += (new Binomial(1, activatedOut)).draw
        }
        (activationStore, DenseVector(sampleVec.map(_.toDouble).toArray))

      case Hidden() =>
        val sampleVec = ListBuffer[Int]()
        val activationStore = DenseVector.zeros[Double](destLayer.numNeurons)
        for (i <- 0 until destLayer.numNeurons) {
          //println(i)
          //println("Input:"+input)
          //println("weight"+weight(i, ::).t)
          val sigma = (input.toDenseMatrix.t * weight.t(i, ::))   //
          val sigmaPlusBias = sigma(0,0)+ hbias(i)
          println(sigmaPlusBias)
          val activatedOut = Activations.sigmoid(sigmaPlusBias)
          activationStore(i) = activatedOut
          sampleVec += (new Binomial(1, activatedOut)).draw
        }
        (activationStore, DenseVector(sampleVec.map(_.toDouble).toArray))

      case _ =>
        println("Error: Unknown/Unsupported layer type..!!")
        (null, null)
    }
  }
  def contrastiveDiv(
    input: DenseVector[Double],
    learningRate: Double,
    numTrainSamples: Int,
    numIterations: Int = 2,
    numHidden: Int = this.numHidden,
    numVisible: Int = this.numVisible) { // This is an approximate GD using mcmc 

    // 1. Calculate the first positive phase..
    val (posHidActVec, posHidSample) = computeActivAndSample(hiddenLayer, input) // This is first positive phase

    // 2. Gibb's sampling for numIterations..
    println("Doing gibb's sampling....")
    
    
    val (hiddenSample, hiddenSigmoid, visibleSample, visibleSigmoid) = Iterator.iterate(posHidSample, posHidActVec, DenseVector.zeros[Double](numHidden), DenseVector.zeros[Double](numHidden)) {
      case (hidSample, hidSigmoid, visSample, visSigmoid) =>
        gibbsSampling(hidSample)
    }.drop(numIterations - 1).next

    
    println("Adjusting weights...")
    println(numHidden)
    // 3. Adjust weights
    println("Dumping vars..")
    
    println(s"posHidActVec $posHidActVec , input $input hiddenSigmoid $hiddenSigmoid visibleSample $visibleSample")
    
    
    for {
      i <- 0 until numHidden
      j <- 0 until numVisible
    }
     {
      //weight(i,j) += 
      learningRate * (posHidActVec(i) * input(j) - hiddenSigmoid(i) * visibleSample(j)) / numTrainSamples
    }
    
    
    // 4. Adjust hidden bias
    (0 until numHidden).foreach { i =>
      hbias(i) = learningRate * (posHidSample(i) - hiddenSigmoid(i)) / numTrainSamples
    }

    // 5. Adjust visible bias
    (0 until numVisible).foreach { i =>
      vbias(i) = learningRate * (input(i) - visibleSample(i)) / numTrainSamples
    }

  }

  def reconstructInput(input: DenseVector[Double]): DenseVector[Double] = {
    val vToH = (calcActivationEnergy(input) :+ hbias)
      .map(Activations.sigmoid) // Go up 
    (calcActivationEnergy(vToH, false) :+ vbias)
      .map(Activations.sigmoid) // and ............>>>>>> down
  }

}
object RBM {

  def testRBM {
    val learningRate = 0.1
    val trainingEpochs = 1000
    val numIters = 1

    val trainNum: Int = 3;
    
    val numVis: Int = 6
    val numHid: Int = 3

    val visLayer = RBMLayer(numVis, Visible(), DenseVector.zeros(numVis))
    val hidLayer = RBMLayer(numHid, Hidden(), DenseVector.zeros(numHid))

    val trainInp:List[DenseVector[Double]] = List(
      new DenseVector(Array(1, 1, 1, 0, 0, 0)),
      new DenseVector(Array(1, 0, 1, 0, 0, 0)),
      new DenseVector(Array(1, 1, 1, 0, 0, 0)),
      new DenseVector(Array(0, 0, 1, 1, 1, 0)),
      new DenseVector(Array(0, 0, 1, 0, 1, 0)),
      new DenseVector(Array(0, 0, 1, 1, 1, 0)))

      
      
      
    val rbm = new RBM(visLayer, hidLayer)
    println("Initializing weights..")
    rbm.initWeights
    println(rbm.weight)
   
    
    println("Training .....")
    trainInp.foreach(rbm.contrastiveDiv(_, learningRate,trainNum))
    
    
    println("Testing")
    val testV = new DenseVector(Array(1.0, 1, 0, 0, 0, 0)) 
    println("Test input:"+testV.toString)
    println("Reconstructed Input:"+rbm.reconstructInput(testV))
    

  }

  def main(args: Array[String]): Unit = {
    testRBM
  }
}