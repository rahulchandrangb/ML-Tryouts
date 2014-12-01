package rahul.NLP.datastructs
import breeze.linalg.{ DenseMatrix, DenseVector }
import breeze.stats.distributions.Uniform
import scala.collection.mutable.ListBuffer

class Tree(val leftChild: Tree,
  val rightChild: Tree,
  val value: DenseVector[Double],
  val isLeaf: Boolean = false,
  val hasSibling: Boolean = true) {

  val leafNodes = ListBuffer[(String, DenseVector[Double])]()
  def insertWordVec(wvec: (String, DenseVector[Double])) {
    if (isLeaf) leafNodes += wvec
    else println("Error:Trying to insert leaf on an intermediate node.!!!")
  }

  def constructTreeFromLeaf: Tree = {

    return null
  }

  def calculateScore: Double = {
    (new Uniform(0, 5)).draw //Later change it to score function
  }
  def getParentVector(l: DenseVector[Double], r: DenseVector[Double]): DenseVector[Double] = {
    //For now we'll give some vector , later we'll implement tanh 
    DenseVector.rand(l.length)
  }

}