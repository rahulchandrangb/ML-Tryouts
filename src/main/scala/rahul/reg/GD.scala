package rahul.reg
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.io.Source





object test extends App{
  
  
  
  def testType(a:Any)= a match{
    case x if x.isInstanceOf[DenseMatrix[Any]] =>
      println("DenseMatrix")
    case _ =>
      println("cannot infer")
  }
  
}