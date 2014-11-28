package rahul.NLP

import scala.collection.immutable.HashSet
import breeze.linalg.{DenseMatrix,DenseVector}

/*
 * 
 * Represents a Recursive Neural Tensor Network
 * Details can be found at: http://nlp.stanford.edu/~socherr/EMNLP2013_RNTN.pdf
 */
class RNTN(val wordRDD: Map[String, Array[Double]], //Word2Vec representation as map
  val featureSize: Int // Vector size of each word
  ) {

}
object RNTN {

}

class RecursiveInput(
  val activationVector:HashSet[String], //Contains a set of all vocabulary in the docs
  val adjacencyMatrix:DenseMatrix[Boolean] // Symmetric matrix , true if words are adjacent 
  ){
  
}