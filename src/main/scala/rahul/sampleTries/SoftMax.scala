package rahul.sampleTries
import scala.math._
import breeze.linalg.{ DenseMatrix, DenseVector }

class LabelPredict(val numInp: Int, val numOut: Int) {
  val weightMatrix = DenseMatrix.zeros[Double](numOut, numInp)

  val bias = DenseVector.zeros[Double](numOut)
  private var learningRate = 0.1

  def setLearningRate(lr: Double) {
    learningRate = lr
  }

  def softmax(classSet: DenseVector[Double]) = {
    val maxVal = classSet.max
    val expMap = classSet.map(curVal => exp(curVal - maxVal))
    val sigmaExp = expMap.sum
    expMap.map(_ / sigmaExp)
  }
  // Batch train method. Maybe we need serial method also.
  def train(inpBatch: DenseMatrix[Double], actBatchOut: DenseMatrix[Int], trainSampleSize: Int) {
    /* 
	   * This method updates weights and bias based on the training set.The method uses batch updates.
	   * Note: inpBatch  and  actBatchOut contains each x(i) in the columns, as breeze file reading is column major
	   * 
	   * @param inpBatch Input values as batch
	   * @param actBatchOut Expected output class in binary form
	   * @param trainSampleSize The total size of training set 
	   * 
	   */
    val classProb = (weightMatrix * inpBatch) // Represents P(Y|X) 
    val softmaxBatch = DenseMatrix.zeros[Double](classProb.rows, classProb.cols)

    // 1. Feed forward,get o/p
    (0 until classProb.cols) map {
      idx =>
        classProb(::, idx) += bias
        softmaxBatch(::, idx) += softmax(classProb(::, idx).toDenseVector)
    }

    // 2. Calculate error
    val delMat = actBatchOut.map(_.toDouble) - classProb // represents y-P(Y|x)

    // 3. Weight Update
    for {
      colIdx <- 0 until delMat.cols
      rowIdx <- 0 until delMat.rows
    } {
      val xx = inpBatch(::, colIdx) :* ((delMat(rowIdx, colIdx) * learningRate) / trainSampleSize)
      weightMatrix(rowIdx, ::) += xx.t
    }

    // 4. Bias update

    (0 until delMat.cols) foreach {
      idx =>
        bias += delMat(::, idx).toDenseVector * (learningRate / trainSampleSize)
    }
  }
  
  def predict(x:DenseVector[Double]):DenseVector[Double]={
    softmax((weightMatrix * x) + bias)
  }

}