package rahul.NLP.datastructs
import breeze.linalg.{ DenseMatrix, DenseVector }
import breeze.stats.distributions.Uniform
import scala.collection.mutable.ListBuffer
import rahul.NLP.NeuralNet

/*
 * Description:
 * =============
 * Tree class represents the tree data structure used to store 
 * word vectors in the leaf and calculated vectors of it's parent.
 * The tree is a binary tree, with each node[subtree] carries it's corresponding vector representation.
 * This is used for recursive neural network's implementation. 
 * For Recursive neural Tensor Network, see rahul.NLP.datastructs.RNTNTree class
 * 
 * For more details  about RNN and RNTN, see socher et el.
 * 
 * @param leftChild The left child sub tree
 * @param rightChild The right child subtree
 * @param value Word vector value of the tree
 * 
 * @author Rahul Chandran
 * 
 */
class Tree(
  val leftChild: Tree,
  val rightChild: Tree,
  val value: DenseVector[Double],
  val hasSibling: Boolean = true) {

  private var parent: Tree = null;

  val leafNodes = ListBuffer[(String, DenseVector[Double])]()
  
  /*
   * Checks whether the tree is a leaf node or not
   */
  def isLeaf = ((leftChild==null) && (rightChild==null))
  
  /*
   * Calculate the number of child trees
   */
  def numChildren = {
	  if(isLeaf) 0
	  else if(leftChild==null) 1
	  else 2
  }
  def insertWordVec(wvec: (String, DenseVector[Double])) {
    if (isLeaf) leafNodes += wvec
    else println("Error:Trying to insert leaf on an intermediate node.!!!")
  }
  
  private[NLP] def setParent(parentTree: Tree) {
    parent = parentTree
  }
  
  /*
   * Used to insert the parent node for two nodes.
   */
  private[NLP] def insertParentNode(leftChild: Tree, rightChild: Tree, value: DenseVector[Double]): Tree = {
    val parTree = new Tree(leftChild, rightChild, value)
    leftChild.setParent(parTree)
    rightChild.setParent(parTree)
    parTree
  }
  
}