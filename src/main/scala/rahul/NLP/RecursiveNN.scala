package rahul.NLP
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import breeze.linalg.{DenseMatrix,DenseVector}


class RecursiveNN(
  word2vec: Map[String, List[Double]],
  leanringRate: Double,
  windowSize: Int) {

  val vectorSize = word2vec.head._2.size
  //val scoreUMatrix = DenseMatrix.rand(rows, cols, rand)
  
  
  
  @tailrec
  private def convertToWindowBlocks(data:Iterator[String],windowSlide:Iterator[List[String]] = Iterator()):Iterator[List[String]]={
    if(data.hasNext){
      val window = data.take(windowSize).toList
      convertToWindowBlocks(data,windowSlide ++ Iterator(window))
    }
    else windowSlide
  }
  def train(data: Iterator[String]) = {
    val blockVectorData = convertToWindowBlocks(data).map{
      dt =>
        dt.map(word2vec.getOrElse(_, null))
    }
  }
  
  def forward(){  //Note the use of [U.transpose] <- Score calc matrix/function
    
  }

}
