package rahul.NLP
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import breeze.linalg.{DenseMatrix,DenseVector}
import rahul.NLP.datastructs.Tree
import breeze.stats.distributions.Gaussian

class RecursiveNNDemo(
  word2vec: Map[String, List[Double]],
  leanringRate: Double,
  windowSize: Int) {

  val vectorSize = word2vec.head._2.size
  //val scoreUMatrix = DenseMatrix.rand(rows, cols, rand)
  val gauRng = new Gaussian(0,0.1)
  val Umatrix = DenseVector.zeros[Double](2).map(_ => gauRng.draw) //To calculate score
  val sampleWeightMat = DenseVector.zeros[Double](2).map(_ => gauRng.draw)
  
  val initNN = new NeuralNetForVector(new DenseMatrix(1,2,sampleWeightMat.toArray),DenseVector(1.0))

  @tailrec
  private def convertToWindowBlocks(data:Iterator[String],windowSlide:Iterator[List[String]] = Iterator()):Iterator[List[String]]={
    if(data.hasNext){
      val window = data.take(windowSize).toList
      convertToWindowBlocks(data,windowSlide ++ Iterator(window))
    }
    else windowSlide
  }
  def train(data: Iterator[String]) = {  //data is the corpus with each word as element of Iterator
    val blockVectorData = convertToWindowBlocks(data).map{
      dt =>
        (dt.map(word2vec.getOrElse(_, null)))
    }
    
  }
  
  def createParent(leftTree:Tree,rightTree:Tree):Tree = {  //Note the use of [U.transpose] <- Score calc matrix/function
    val parentVec = initNN.calculateParentVec(leftTree.value, rightTree.value)
    val score = Umatrix.t * parentVec
    val parentTree = new Tree(leftTree,rightTree,parentVec)
    parentTree.setScore(score)
    parentTree
  }

}
object Test extends App{
  val inpWord = List("cat","sat","on", "a","mat")
  val inpVec = List(List(1.0,2.0),List(9.0,6),List(1.0,3.0),List(3.0,6.0),List(5.0,4.0))
  val inpMap = inpWord.zip(inpVec).toMap
  println("Input map:\n===================\n"+inpMap.mkString("|"))
  println("=================================")
  val rnnInst = new RecursiveNNDemo(inpMap,0.1,5)
}

