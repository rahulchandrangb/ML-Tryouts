package rahul.NLP.datastructs
import breeze.linalg.{ DenseMatrix, DenseVector }
import scala.collection.mutable.ListBuffer

class Tree {
  val leafNodes = ListBuffer[(String, DenseVector[Double])]()
  
  def insertWordVec(wvec: (String, DenseVector[Double])) {
    leafNodes += wvec
  }
  
  def constructTree{
    
  } 
  
  def calculateScore={
    
  }
  def getParentVector={
    
  }
}