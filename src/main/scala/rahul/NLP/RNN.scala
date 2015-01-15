package rahul.NLP
import breeze.linalg.{ DenseVector, DenseMatrix }
import breeze.stats.distributions.Gaussian
import rahul.NeuralNetwork.LabelPredict
import rahul.NLP.datastructs.Tree
class RNN(vecLen: Int, wRowColSize: (Int, Int), labelMap: Map[String, Int], word2vec: Map[String, DenseVector[Double]]) {
  // 1.List of words ..Goes in train method

  // 2. Word2Vec ..Goes in train method
  val gauRng = new Gaussian(0, 0.1)
  // 3. W
  val W = DenseMatrix.zeros[Double](wRowColSize._1, wRowColSize._2).map(_ => gauRng.draw) //Represents the weight of the neural networks
  // 4. Wscore
  val U = DenseMatrix.zeros[Double](vecLen, 1).map(_ => gauRng.draw) //To calculate score
  //Softmax Layer for LabelPrediction
  val softmaxLayer = new LabelPredict(vecLen, labelMap.size)

  def init() { //We'll add optimization code here later..
    
  }
  def train(inputVec: Iterator[(List[Double],Tree)]) { //Input : Iterator of tuple of Word Vecs of each word and the corresponding truth tree 
    // 1. Construct Predicted tree
    // 2. Traverse through truth tree to generate parent vectors.
    // 3. Back Propagation through Structure
    //    (3.a) Label Back propagation part
    //    (3.b) Structural back propagation 
    
    // 4. Sum the collected deltas and add to parameters. // We should also try LBGFS here
  } 
  
  def forwardPropagate(input:DenseVector[Double]):DenseVector[Double]={ // Implement the method here..
    
    null
  }
  
  def calculateError:Double = {
     1.0
  }
  
  def predict(strList:List[String])={
    
  }
  
}