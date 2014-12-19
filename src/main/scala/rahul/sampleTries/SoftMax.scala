package rahul.sampleTries
import scala.math._
import breeze.linalg.{DenseMatrix,DenseVector}


class LabelPredict(val numInp:Int,val numOut:Int) {
	val weightMatrix = DenseMatrix.zeros[Double](numOut,numInp)
	
	val bias  = DenseVector.zeros[Double](numOut)
    private var learningRate = 0.1
    
    def setLearningRate(lr:Double){
	  learningRate = lr
	}
    
	def softmax(classSet:DenseVector[Double]) = {
	  val maxVal = classSet.max
	  val expMap = classSet.map(curVal => exp(curVal-maxVal))
	  val sigmaExp = expMap.sum
	  expMap.map(_/sigmaExp)  
	}
	
	def train(inpBatch:DenseMatrix[Double],actBatchOut:DenseMatrix[Int],trainSampleSize:Int)={
	  val classProb = (weightMatrix * inpBatch) // Represents P(Y|X)
	  val softmaxBatch = DenseMatrix.zeros[Double](classProb.rows, classProb.cols)
	  
	  (0 until classProb.cols) map {
	    idx =>
	      classProb(::,idx) += bias
	      softmaxBatch(::,idx) += softmax(classProb(::,idx).toDenseVector)
	  }
	  
	  val deltaOut = DenseVector.zeros[Double](numOut)   // represents y-P(Y|x)
	  val delMat = actBatchOut.map(_.toDouble) - classProb
	  
	  
      /*
       * 
    val dy: Array[Double] = new Array[Double](n_out)
    for(i <- 0 until n_out) {
      dy(i) = y(i) - p_y_given_x(i)

      for(j <- 0 until n_in) {
        W(i)(j) += lr * dy(i) * x(j) / N
      }
      b(i) += lr * dy(i) / N
    }
       * 
       * 
       */
	}

}