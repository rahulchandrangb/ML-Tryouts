package rahul.NLP
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import breeze.linalg.{ DenseMatrix, DenseVector }
import rahul.NLP.datastructs.Tree
import breeze.stats.distributions.Gaussian
import rahul.NeuralNetwork.PSelectStrategy
import rahul.NeuralNetwork.GreedySelect

class RecursiveNNDemo(
  val wVec: List[(String, List[Double])], // reference word to vec 
  val leanringRate: Double, //specifies the learning rate
  val windowSize: Int, //specifies the window size.
  val parentSelStrategy: PSelectStrategy = GreedySelect()) {

  val word2vec = wVec.toMap
  val vectorSize = word2vec.head._2.size

  val gauRng = new Gaussian(0, 0.1)
  val Umatrix = DenseVector.zeros[Double](2).map(_ => gauRng.draw) //To calculate score
  val sampleWeightMat = DenseVector.zeros[Double](2).map(_ => gauRng.draw)

  val initNN = new NeuralNetForVector(new DenseMatrix(1, 2, sampleWeightMat.toArray), DenseVector(1.0))

  @tailrec
  private def convertToWindowBlocks(data: Iterator[String], windowSlide: Iterator[List[String]] = Iterator()): Iterator[List[String]] = {
    if (data.hasNext) {
      val window = data.take(windowSize).toList
      convertToWindowBlocks(data, windowSlide ++ Iterator(window))
    } else windowSlide
  }

  def train(data: Iterator[String]) = { //data is the corpus with each word as element of Iterator
    convertToWindowBlocks(data).map(_.map(word2vec.getOrElse(_, null)))
  }

  def createParent(leftTree: Tree, rightTree: Tree): Tree = {
    val parentVec = initNN.calculateParentVec(leftTree.value, rightTree.value) //This part replace with ANN forward method
    val score = Umatrix.t * parentVec //This part replace with ANN forward method

    val parentTree = new Tree(leftTree, rightTree, parentVec)
    leftTree.setParent(parentTree)
    rightTree.setParent(parentTree)
    parentTree.setScore(score)
    parentTree
  }
  def constructRootNode(leafList: List[Tree]): Tree = {
    createBestScoreTree(leafList)(0)
  }
  @tailrec
  private def createBestScoreTree(inp: List[Tree]): List[Tree] = {
    if (inp.length == 1) inp
    val parentList = inp.zip(inp.tail).map {
      case (lt: Tree, rt: Tree) =>
        createParent(lt, rt)
    }
    val maxTree = parentList.reduce((a, b) => if (b.getScore > a.getScore) b else a)
    val outTrees = inp.map {
      node =>
        if (node == maxTree.leftChild) {
          maxTree
        } else if (node == maxTree.rightChild) null
        else node
    }.filter(_ != null)
    createBestScoreTree(outTrees)
  }

}
object Test extends App {
  val inpWord = List("cat", "sat", "on", "a", "mat")
  val inpVec = List(List(1.0, 2.0), List(9.0, 6), List(1.0, 3.0), List(3.0, 6.0), List(5.0, 4.0))
  val inpMap = inpWord.zip(inpVec)
  println("Input map:\n===================\n" + inpMap.mkString("\n"))
  println("=================================")
  val rnnInst = new RecursiveNNDemo(inpMap, 0.1, 5)
  println("Trying to create first level parent output...")
  println("Umatrix:\n" + rnnInst.Umatrix.toString)
  println("\n\n")
  println("weight matrix:" + rnnInst.sampleWeightMat)
  println("-------------------------------")

  val treeList: List[Tree] = inpMap.map(x => new Tree(null, null, new DenseVector(x._2.toArray), x._1)).toList
  val treeList2: List[Tree] = treeList.tail :+ null

  val parentList = treeList.zip(treeList2).init.map {
    case (lt: Tree, rt: Tree) =>
      rnnInst.createParent(lt, rt)
  }
  println(parentList.mkString("\n"))
  Tree.toDot("/tmp/outdot.ps", "name", parentList)

}

