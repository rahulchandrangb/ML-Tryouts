package rahul.reg
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.io.Source

class GDesc(srcFile:String,learningRate:Double){
	def getDelta={
	  val lines = Source.fromFile(srcFile).getLines.toList
	  
	  val xVals = lines.map(_.split(",")(0).toInt)
	  val yVals = lines.map(_.split(",")(1).toInt)
	  
	  
	}
	
	def sqError(target:List[Double],calOutput:List[Double])={
	  
	}
	
}