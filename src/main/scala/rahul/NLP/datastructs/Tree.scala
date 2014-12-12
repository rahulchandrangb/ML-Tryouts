package rahul.NLP.datastructs
import breeze.linalg.{ DenseMatrix, DenseVector }
import breeze.stats.distributions.Uniform
import scala.collection.mutable.ListBuffer
import scala.collection._
import scala.annotation.tailrec
import java.io.PrintWriter
import java.io.File
import scala.sys.process._

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
  val label: String = null) {

  private var parent: Tree = null; // For now we'll make it mutable, later replace it with persistent tree
  private var score: Double = -200000000.0
  private val parents = ListBuffer[Tree]()
  private var parCurIdx = 0
  val name: String =
    if (label == null)
      "(" + leftChild.name + ")::(" + rightChild.name + ")"
    else
      label

  val leafNodes = ListBuffer[(String, DenseVector[Double])]()

  /*
   * Checks whether the tree is a leaf node or not
   * 
   */

  def isLeaf = ((leftChild == null) && (rightChild == null))

  def getParent = {
    if (isLeaf) {
      if (parCurIdx >= parents.size) parCurIdx = 0
      val tr = parents(parCurIdx)
      parCurIdx += 1
      tr
    } else {
      parent
    }
  }
  /*
   * Calculate the number of child trees
   */

  def setNewValue(value: DenseVector[Double]) = {
    require(value.size == this.value.size, "Size mismatch between existing value vector and new vector!")
    0 until value.size map {
      v =>
        this.value(v) = value(v)
    }
  }
  def setScore(scoreval: Double) {
    score = scoreval
  }
  def numChildren = {
    if (isLeaf) 0
    else if (leftChild == null) 1 // Error .. shudn't have reached here..!!
    else 2
  }

  def getScore=score
  def getVectorSize = value.size

  def getChildTokens(tree: Tree, labelList: List[String] = List()): List[String] = {
    if (tree.isLeaf) labelList :+ tree.label
    else {
      val leftTokenList = getChildTokens(tree.leftChild, labelList)
      val rightTokenList = getChildTokens(tree.rightChild, labelList)
      leftTokenList ++ rightTokenList
    }
  }

  def getLeafList(tree: Tree, leafList: List[Tree] = List()): List[Tree] = {
    if (tree.isLeaf) leafList :+ tree
    else {
      val leftTokenList = getLeafList(tree.leftChild, leafList)
      val rightTokenList = getLeafList(tree.rightChild, leafList)
      leftTokenList ++ rightTokenList
    }
  }

  private[NLP] def setParent(parentTree: Tree) {
    parent = parentTree
    if (isLeaf) parents += parentTree
  }

  /*
   * This method converts tree to a dot file, which will be easy to visualize later
   * 
   *  @param fileLoc The file location to write the dot and ps file
   *  @param nodeType The node value to appear in nodes of graphviz, 
   *    the allowed values are name,score,vector. By default it is name
   *    
   */

  def toDot(psLoc: String = "/tmp/outdot.ps", nodeType: String = "name") {
    val fileLoc = if (psLoc endsWith ".ps") psLoc.substring(0, psLoc.length - 2) + "dot" else psLoc + ".dot"
    //val dotMap = mutable.Map[String, String]()
    val writer = new PrintWriter(fileLoc)
    writer.println("digraph rnn{")

    val leafs: ListBuffer[Tree] = getLeafList(this).to[ListBuffer]
    while (leafs.size > 1) {

      leafs.foreach {
        x =>
          val (dispKey: String, dispValue: String) = nodeType match {
            case "name" =>
              println(x.name)
              (x.name, x.parent.name)
            case "score" =>
              (x.name + " :::score:" + x.score, x.parent.name + " :::score:" + x.score)
            case "vector" =>
              (x.name, x.parent.name)
          }
          writer.println('"' + dispKey + '"' + "  ->  " + '"' + dispValue + '"' + ';')
      }
      leafs.clear
    }
    writer.println("}")
    writer.close
  }

  /*
   * Used to insert the parent node for two nodes.
   */

  //override def toString ="==========================\nname:"+name+"\nscore:"+score+"\nvalue:\n--------\n"+value.toString+"\n========================\n"
  override def toString = "name:" + name + ", score:" + score

}
object Tree {
   def createParent(leftTree: Tree, rightTree: Tree,parentVec:DenseVector[Double],score:Double): Tree = {
    val parentTree = new Tree(leftTree, rightTree, parentVec)
    leftTree.setParent(parentTree)
    rightTree.setParent(parentTree)
    parentTree.setScore(score)
    parentTree
  }
  def toDot(psLoc: String = "/tmp/outdot.ps", nodeType: String = "name", treeList: List[Tree]) {
    val fileLoc = if (psLoc endsWith ".ps") psLoc.substring(0, psLoc.length - 2) + "dot" else psLoc + ".dot"
    //val dotMap = mutable.Map[String, String]()
    val writer = new PrintWriter(fileLoc)
    writer.println("digraph rnn{")

    treeList.foreach {
      tree =>
        val leafs: ListBuffer[Tree] = tree.getLeafList(tree).to[ListBuffer]
        while (leafs.size > 1) {
          val newLeaves = leafs.map {
            x =>
              x.parent match {
                case null =>
                  null
                case _ =>
                  val (dispKey: String, dispValue: String) = nodeType match {
                    case "name" =>
                      (x.name, x.getParent.name)
                    case "score" =>
                      (x.name + " :::score:" + x.score, x.getParent.name + " :::score:" + x.getParent.score)
                    case "vector" =>
                      (x.name, x.getParent.name)
                  }

                  writer.println('"' + dispKey + '"' + "  ->  " + '"' + dispValue + '"' + ';')
                  x.parent
              }
          }

          leafs.clear
          leafs ++= (newLeaves.filter(_ != null))
        }
    }
    writer.println("}")
    writer.close
    val dotCmd = s"dot -Tps $fileLoc -o $psLoc"
    dotCmd.!!
    println(s"Generated ps file: $psLoc")
  }
}