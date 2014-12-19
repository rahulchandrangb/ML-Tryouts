package rahul.NLP

import rahul.NLP.datastructs.Tree
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import breeze.linalg.DenseVector
import collection.mutable.Map
object ParseTests { //regex match not workign..need to check
  val str = "(S (NP (NNP Stanford) (NNP University)) (VP (VBZ is) (VP (VBN located) (PP (IN in) (NP (NNP California))))))"
  val finalTree = parseToTree(str)
  println(finalTree)

  def parseToTree(str: String, cint: Int = 0, lookupMap: Map[Int, Tree] = Map(), tr: Tree = null): Tree = {
    println(str)
    val childRegex = """\\((\\w+)\\s+(\\w+)\\)""".r
    val parentRegexWithOneChild = """\\((\\w+)\\s+\\[(\\d+)\\]\\)""".r
    val parentRegexWithTwoChild = """\\((\\w+)\\s+\\[(\\d+)\\]\\s+\\[(\\d+)\\]\\)""".r
    if (str.isEmpty()) tr
    str match {
      case childRegex(label: String, name: String) =>
        lookupMap += ((cint + 1) -> new Tree(null, null, DenseVector(1.0, 2.0), label))
        parseToTree(str.replaceFirst(childRegex.toString, "[" + (cint + 1) + "]"), cint + 1, lookupMap, tr)

      case parentRegexWithOneChild(label: String, idx: String) =>
        val tree = new Tree(lookupMap.getOrElse(idx.toInt, null), null, DenseVector(1.0, 2.0), label) //One node tree
        lookupMap += ((cint + 1) -> tree)
        parseToTree(str.replaceFirst(parentRegexWithOneChild.toString, "[" + (cint + 1) + "]"), cint + 1, lookupMap, tree)

      case parentRegexWithTwoChild(label: String, lftChildIdx: String, rightChildIdx: String) =>
        val tree = new Tree(lookupMap.getOrElse(lftChildIdx.toInt, null), lookupMap.getOrElse(rightChildIdx.toInt, null), DenseVector(1.0, 2.0), label)
        parseToTree(str.replaceFirst(parentRegexWithOneChild.toString, "[" + (cint + 1) + "]"), cint + 1, lookupMap, tree)
      case _ =>
        println("unmatched")
        tr
    }
  }
}
