package rahul.NeuralNetwork
import Activations._
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.math._
import scala.beans.BeanProperty
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.annotation.tailrec
import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Uniform

class MultiLayerNN(val layers: List[ANNLayer],
  val weightMatrices: ListBuffer[DenseMatrix[Double]] = ListBuffer[DenseMatrix[Double]](),
  val learningRate: Double = 0.2, // Determines the fraction of weight and bias change [beta]
  val momentum: Double = 0.3, // To prevent local minima during GD [Introduce weight decay][alpha]
  val activationFunc: Double => Double = Activations.sigmoid,
  val derivActivationFunc: Double => Double = Activations.derivSigmoid) extends Network {
  val activatedInputList = ListBuffer[DenseVector[Double]]()
  val deltaVectorList = ListBuffer[DenseVector[Double]]()

  private var input: DenseVector[Double] = null
  private var output: DenseVector[Double] = null

  def setInpOutp(inp: DenseVector[Double], out: DenseVector[Double]) = {
    input = inp
    output = out
  }

  val layerSz = layers.size

  def initialize(boolInitWeight: Boolean = true) {
    //1. Initialize collection vectors
    (0 until layerSz).map {
      x =>
        activatedInputList += DenseVector()
        deltaVectorList += DenseVector()
    }
    //2. Initialize weight Matrix
    if (boolInitWeight) initWeightMatrix
  }

  def train(strategy: TrainStrategy = TrainByEpoch(Data.DEFAULT_TRAIN_EPOCHS)) {
    strategy match {
      case TrainByEpoch(numEpoch) =>
        (0 until numEpoch).foreach {
          loop =>
            println(s"Epoch: $loop")

        }
      case TrainByError(numErr) =>

    }
  }

  private def initWeightMatrix { // Initialize with a Uniform Dist or Normal dist..??
    if (weightMatrices.size == 0) { //Weight Matrices not even created
      layers.foreach(x => weightMatrices += DenseMatrix.zeros[Double](x.numNeurons, x.numInp))
    }
    (0 until weightMatrices.size).foreach {
      idx =>
        val matrix = weightMatrices(idx)
        val u = new Uniform(0, 1)
        weightMatrices(idx) = matrix.map(_ => u.sample)
    }
  }

  def getActivation(layernum: Int, curInputs: DenseVector[Double]): DenseVector[Double] = {
    val weightMatOfLayer = weightMatrices(layernum)
    val nextLayerNeuronCnt =
      if (layernum < layers.size - 1)
        layers(layernum + 1).numNeurons
      else
        1
    val activationVect = DenseVector.zeros[Double](nextLayerNeuronCnt)
    for (i <- 0 until nextLayerNeuronCnt) {
      activationVect(i) = activationFunc(
        curInputs.toArray.zipWithIndex
          .map(x => weightMatOfLayer(i, x._2) * x._1)
          .reduce(_ + _) + layers(layernum).bias(i))
    }
    activationVect
  }
  def predict(inp: DenseVector[Double]): DenseVector[Double] = {
    null
  }
  def computeSquaredError(target: List[Double], computedOut: List[Double]): Double = {
    (target.zip(computedOut).map(x => pow((x._1 - x._2), 2)).reduce(_ + _)) / 2 // blas densevector multiplication can also be used for memory efficient computation    
  }

  def calculateOutput = forward(0, input)

  @tailrec
  private def forward(layerNum: Int = 0, curInputs: DenseVector[Double] = input): DenseVector[Double] = {
    if (layerNum == layers.size) curInputs
    else {
      val newInp = getActivation(layerNum, curInputs)
      activatedInputList(layerNum) = newInp //Side effect !!!!
      forward(layerNum + 1, newInp)
    }
  }
  def backPropagate {
    updateDeltaList() //Propagate and find the delta for all layers 
    updateWt() //Adjust the weight matrices
    updateBias //Adjust the bias
  }

  @tailrec
  private def updateDeltaList(layer: ANNLayer = layers.last): Unit = {
    //Check whether it's output layer  
    val deltaVector: DenseVector[Double] = layer.layerType match {
      case Output() =>
        DenseVector(activatedInputList(layer.layerIdx).toArray
          .zipWithIndex.map { x => derivActivationFunc(x._1) * (output(x._2) - x._1) })
      case _ =>
        val deltaAlone = (deltaVectorList(layer.layerIdx + 1).toDenseMatrix * weightMatrices(layer.layerIdx + 1)).toDenseVector
        val derivOutput = activatedInputList(layer.layerIdx).map(derivActivationFunc)
        deltaAlone :* derivOutput
    }
    deltaVectorList(layer.layerIdx) = deltaVector
    if (layer.layerIdx - 1 == 0) return
    else updateDeltaList(layers(layer.layerIdx - 1))
  }

  @tailrec
  private def updateWt(layer: ANNLayer = layers.last) {
    val weightMat = weightMatrices(layer.layerIdx)
    val newWeightArr = (for {
      i <- 0 until weightMat.rows
      j <- 0 until weightMat.cols
    } yield (weightMat(i, j) + (deltaVectorList(layer.layerIdx)(j) * learningRate * activatedInputList(layer.layerIdx - 1)(j)))).toArray
    weightMatrices(layer.layerIdx) = new DenseMatrix(weightMat.rows, weightMat.cols, newWeightArr)
    if (layer.layerIdx == 0) return
    else updateWt(layers(layer.layerIdx - 1))
  }

  private def updateBias {
    (0 until layers.size).map {
      idx =>
        deltaVectorList(idx).toArray.zipWithIndex.map {
          del =>
            layers(idx).setBiasVal(del._2, del._1)
        }
    }
  }
}

object MultiLayerNN {
  def loadModelFromFile(srcFile: String): MultiLayerNN = {
    val inputList: List[(String, Int)] = Source.fromFile(srcFile).getLines.zipWithIndex.toList
    val lastElemIdx = inputList.size - 1
    val layerList: List[ANNLayer] = inputList.map {
      x =>
        if (x._2 == 0) { //First element?  get input, set LayerType as Input
          val lyrDtSplit = x._1.split(",")
          val numNeurons = lyrDtSplit(0).toInt
          val numInp = lyrDtSplit(1).toInt
          ANNLayer(numNeurons, numInp, Input())
        } else if (x._2 == lastElemIdx) { // Last element..So Output layer
          ANNLayer(x._1.toInt, layerType = Output(), layerIdx = x._2)
        } else { // Hidden Layer
          ANNLayer(x._1.toInt, layerIdx = x._2)
        }
    }
    new MultiLayerNN(layerList)
  }
}

