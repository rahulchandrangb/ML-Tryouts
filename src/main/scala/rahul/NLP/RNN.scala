package rahul.NLP
import breeze.linalg.{DenseVector,DenseMatrix}
import rahul.NeuralNetwork.LabelPredict
class RNN(vecLen:Int,wRowColSize:(Int,Int),labelMap:Map[String,Int],word2vec:Map[String,DenseVector[Double]]) {
	// 1.List of words ..Goes in train method
     
    // 2. Word2Vec ..Goes in train method
  
  
  
   // 3. W
   val W = DenseMatrix.zeros[Double](wRowColSize._1,wRowColSize._2) //Represents the weight of the neural networks
   // 4. Wscore
   val U = DenseMatrix.zeros[Double](vecLen,1) // Row Matrix
   
   //Softmax Layer for LabelPrediction
   val softmaxLayer = new LabelPredict(vecLen,labelMap.size)

   
   def init(){
     // init weights
     // init wscore
     // 
   }
   
   
}