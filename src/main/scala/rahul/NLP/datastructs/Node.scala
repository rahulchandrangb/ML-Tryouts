package rahul.NLP.datastructs

import breeze.linalg.DenseVector

case class Node(val leftChild: Node, val rightChild: Node, value: DenseVector[Double])